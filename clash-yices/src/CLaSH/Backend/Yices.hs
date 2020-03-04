{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE InstanceSigs      #-}

module CLaSH.Backend.Yices (YicesState) where

import           Prelude                              hiding ((<$>))

import           Control.Lens                         hiding (Indexed,Context)

import           Text.PrettyPrint.Leijen.Text.Monadic

import           Control.Monad
import           Control.Monad.State                  (State)

import           Data.List                            (intersect,partition)
import           Data.Maybe                           (catMaybes,isNothing,fromMaybe,fromJust)
import           Data.HashSet                         (HashSet)
import qualified Data.HashSet                         as HashSet
import qualified Data.IntMap                          as IntMap
import           Data.Text.Lazy                       (Text)
import qualified Data.Text.Lazy                       as Text
import qualified Data.Text                            as SText

import           CLaSH.Annotations.Primitive          (HDL (..))
import           CLaSH.Backend
import           CLaSH.Driver.Types                   (SrcSpan, noSrcSpan)
import           CLaSH.Netlist.BlackBox.Types         (HdlSyn (..))
import           CLaSH.Netlist.BlackBox.Util          (renderBlackBox,extractLiterals)
import           CLaSH.Netlist.Id                     (mkBasicId')
import           CLaSH.Netlist.Types                  hiding (_intWidth, intWidth)
import           CLaSH.Netlist.Util                   (typeSize)

#ifdef CABAL
import qualified Paths_clash_yices
#else
import qualified System.FilePath
#endif

-- | State for the 'CLaSH.Backend.Verilog.YicesM' monad:
data YicesState =
  YicesState
    { _typesSeen         :: HashSet HWType
    , _typeNames         :: [(HWType,String)]
    , _srcSpan           :: SrcSpan
    , _intWidth          :: Int -- ^ Int/Word/Integer bit-width
    , _currentModule     :: ModName
    , _assignments       :: [(Identifier,Doc)]
    , _otherModules      :: [(Text,Component)]
    , _prefix            :: Text
    , _tempVarI          :: Int
    }

makeLenses ''YicesState

type YicesM a = State YicesState a

instance Backend YicesState where
  -- | Initial state for state monad
  initBackend :: Int -> HdlSyn -> YicesState
  initBackend iw _ = YicesState HashSet.empty [] noSrcSpan iw "" [] [] "" 0
  -- | What HDL is the backend generating
  hdlKind :: YicesState -> HDL
  hdlKind = const Yices
  -- | Location for the primitive definitions
  primDir :: YicesState -> IO FilePath
#ifdef CABAL
  primDir         = const (Paths_clash_yices.getDataFileName "primitives")
#else
  primDir _       = return ("clash-yices" System.FilePath.</> "primitives")
#endif
  -- | Name of backend, used for directory to put output files in. Should be
  -- | constant function / ignore argument.
  name :: YicesState -> String
  name = const "yices"
  -- | File extension for target langauge
  extension :: YicesState -> String
  extension = const "ys"
  -- | Get the set of types out of YicesState
  extractTypes :: YicesState -> HashSet HWType
  extractTypes = _typesSeen
  -- | Generate HDL for a Netlist component
  genHDL :: String -> SrcSpan -> Component -> YicesM ((String, Doc),[(String,Doc)])
  genHDL = genYices
  -- | Generate a HDL package containing type definitions for the given HWTypes
  mkTyPackage :: String -> [HWType] -> YicesM [(String, Doc)]
  mkTyPackage = yicesTypesPackage
  -- | Convert a Netlist HWType to a target HDL type
  hdlType :: HWType -> YicesM Doc
  hdlType = yicesType
  -- | Convert a Netlist HWType to an HDL error value for that type
  hdlTypeErrValue :: HWType -> YicesM Doc
  hdlTypeErrValue = const $ text "<err>"
  -- | Convert a Netlist HWType to the root of a target HDL type
  hdlTypeMark :: HWType -> YicesM Doc
  hdlTypeMark = const $ text "<mark>"
  -- | Create a signal declaration from an identifier (Text) and Netlist HWType
  hdlSig :: Text -> HWType -> YicesM Doc
  hdlSig = yicesSig
  -- | Create a generative block YicesStatement marker
  genStmt :: Bool -> YicesM Doc
  genStmt = undefined
  -- | Turn a Netlist Declaration to a HDL concurrent block
  inst :: Declaration  -> YicesM (Maybe Doc)
  inst = yicesInst
  -- | Turn a Netlist expression into a HDL expression
  expr :: Bool -> Expr -> YicesM Doc
  expr = const yicesExpr
  -- | Bit-width of Int/Word/Integer
  iwWidth :: YicesM Int
  iwWidth = use intWidth
  -- | Convert to a bit-vector
  toBV :: HWType -> Text -> YicesM Doc
  toBV = undefined
  -- | Convert from a bit-vector
  fromBV :: HWType -> Text -> YicesM Doc
  fromBV = undefined
  -- | Synthesis tool we're generating HDL for
  hdlSyn :: YicesM HdlSyn
  hdlSyn = undefined
  -- | mkBasicId
  mkBasicId :: YicesM (Identifier -> Identifier)
  mkBasicId = return (filterReserved . mkBasicId' True)
  -- | setModName
  setModName :: ModName -> YicesState -> YicesState
  setModName n s = s { _currentModule = n }
  -- | setSrcSpan
  setSrcSpan :: SrcSpan -> YicesM ()
  setSrcSpan = (srcSpan .=)
  -- | getSrcSpan
  getSrcSpan :: YicesM SrcSpan
  getSrcSpan = use srcSpan

-- List of reserved Yices keywords
reservedWords :: [Identifier]
reservedWords = [
  "and", "assert", "bit", "bitvector", "bool", "bool-to-bv", "bv-add", "bv-and",
  "bv-ashift-right", "bv-ashr", "bv-comp", "bv-concat", "bv-div", "bv-extract",
  "bv-ge", "bv-gt", "bv-le", "bv-lshr", "bv-lt", "bv-mul", "bv-nand", "bv-neg",
  "bv-nor", "bv-not", "bv-or", "bv-pow", "bv-redand", "bv-redor", "bv-rem",
  "bv-repeat", "bv-rotate-left", "bv-rotate-right", "bv-sdiv", "bv-sge",
  "bv-sgt", "bv-shift-left0", "bv-shift-left1", "bv-shift-right0",
  "bv-shift-right1", "bv-shl", "bv-sign-extend", "bv-sle", "bv-slt", "bv-smod",
  "bv-srem", "bv-sub", "bv-xnor", "bv-xor", "bv-zero-extend", "ceil", "check",
  "define", "define-type", "distinct", "div", "divides", "dump-context",
  "echo", "ef-solve", "eval", "exists", "exit", "export-to-dimacs", "false",
  "floor", "forall", "help", "if", "include", "int", "is-int", "ite", "lambda",
  "let", "mk-bv", "mk-tuple", "mod", "not", "or", "pop", "push", "real",
  "reset", "reset-stats", "scalar", "select", "set-param", "set-timeout",
  "show-implicant", "show-model", "show-param", "show-params", "show-stats",
  "true", "tuple", "tuple-update", "update", "xor"]

tempVar :: YicesM Identifier
tempVar = do
  x <- use tempVarI   
  tempVarI .= x + 1
  return $ Text.append ("_temp_") (Text.pack (show x))

filterReserved :: Identifier -> Identifier
filterReserved s = if s `elem` reservedWords
  then s `Text.append` "_r"
  else s

genYices :: String -> SrcSpan -> Component -> YicesM ((String, Doc),[(String,Doc)])
genYices nm sp c = do
  setSrcSpan sp
  --om <- use otherModules
  currentModule .= nm
  otherModules %= ((componentName c,c) :)
  let cname = componentName c
  let ports' = ports c
  let body = genYicesBody c
  let tmn = Text.pack nm `Text.append` "_types.ys"
  text <-
    --(text $ Text.pack $ show om) <$>
    apply "include" (dquotes $ text tmn) <$>
    ports' <$>
    body
  return ((Text.unpack cname,text),[])

genYicesBody :: Component -> YicesM Doc
genYicesBody c =
  (definitions c) <$>
  (comment "network") <$>
  (vsep $ fmap catMaybes $ mapM yicesInst (declarations c))

yicesType :: HWType -> YicesM Doc
yicesType (BitVector sz) = apply "bitvector" $ int sz
yicesType other = do
  typesSeen %= HashSet.insert other
  yicesTypeName other

yicesTypeName :: HWType -> YicesM Doc
yicesTypeName Void = "Void"
yicesTypeName String = "String"
yicesTypeName Bool = "Bool"
yicesTypeName (BitVector sz) = "BitVector" <> brackets (int sz)
yicesTypeName (Index n) = "Index" <> brackets (int 0 <> ".." <> integer n)
yicesTypeName (Signed sz) = "Signed" <> brackets (int sz)
yicesTypeName (Unsigned sz) = "Unsigned" <> brackets (int sz)
yicesTypeName (Vector sz ty) = "Vector" <> brackets (int sz) <> angles (yicesTypeName ty)
yicesTypeName (RTree sz ty) = "RTree" <> brackets (int sz) <> angles (yicesTypeName ty)
yicesTypeName (Sum name _) = text name
yicesTypeName (Product name tys)
  | Text.isPrefixOf "GHC.Tuple" name = angles (hcat (punctuate comma (mapM yicesTypeName tys)))
  | otherwise = text name
yicesTypeName (SP name cs) = text name
yicesTypeName (Clock name _) = "Clock_" <> text name
yicesTypeName (Reset name _) = "Reset_" <> text name

yicesTypeDef :: HWType -> YicesM Doc
yicesTypeDef Void = "<na>"
yicesTypeDef String = "<na>"
yicesTypeDef Bool = apply "bitvector" $ "1"
yicesTypeDef (BitVector sz) = apply "bitvector" $ int sz
yicesTypeDef (Index n) = "Index" <> brackets (int 0 <> ".." <> integer n)
yicesTypeDef (Signed sz) = apply "bitvector" $ int sz
yicesTypeDef (Unsigned sz) = apply "bitvector" $ int sz
yicesTypeDef (Vector sz ty) = apply "bitvector" $ int (sz * typeSize ty)
yicesTypeDef (RTree sz ty) = "RTree" <> brackets (int sz) <> angles (yicesTypeDef ty)
yicesTypeDef t@(Sum _ constructors) = apply "bitvector" $ int (typeSize t)
yicesTypeDef (Product _ tys) = apply "bitvector" $ int (sum (map typeSize tys))
yicesTypeDef (SP name cs) = text name
yicesTypeDef (Clock name _) = "bool"
yicesTypeDef (Reset name _) = "bool"

yicesTypesPackage :: String -> [HWType] -> YicesM [(String, Doc)]
yicesTypesPackage name tys = do
    x <- vsep (mapM defineType sortedTypes) <$>
         comment "constructors" <$>
         vsep (mapM constructors sortedTypes)
    return [(name ++ "_types",x)]
  where
    sortedTypes = sortTypes tys
    sortTypes tys
      | length tys < 2 = tys
      | otherwise = ok ++ sortTypes rs
        where (ok,rs) = partition (null . intersect tys . nestedTypes) tys
    constructors Bool =
      (apply2 "define" (yicesSig "False" Bool) (apply2 "mk-bv" "1" "0")) <$>
      (apply2 "define" (yicesSig "True" Bool) (apply2 "mk-bv" "1" "1"))
    constructors s@(Sum t cs) = do
      vsep $ forM (zip [0..] cs) $ \(i,c) ->
        apply2 "define" (yicesSig c s) (apply2 "mk-bv" (int (typeSize s)) (int i))
    constructors _ = empty
    defineType ty = apply2 "define-type" (yicesTypeName ty) (yicesTypeDef ty)

  
withAssignments :: [(Identifier,Doc)] -> YicesM a -> YicesM a
withAssignments as b = do
  p <- use assignments
  assignments %= (as ++)
  res <- b
  assignments .= p
  return res

withPrefix :: Text -> YicesM a -> YicesM a
withPrefix pre b = do
  p <- use prefix
  prefix %= Text.append (Text.append pre "_")
  res <- b
  prefix .= p
  return res

prefixed :: Text -> YicesM Doc
prefixed name = do
  as <- use assignments
  case lookup name as of
    Just d -> return d
    Nothing -> do
      p <- use prefix
      text p <> text name

yicesSig :: Text -> HWType -> YicesM Doc
yicesSig name ty = prefixed name <+> dcolon <+> yicesType ty

getItem :: HWType -> Int -> YicesM Doc -> YicesM Doc 
getItem (Vector n ty) k u = bvExtract i j u
  where i = (k + 1) * typeSize ty - 1
        j = k * typeSize ty
getItem (Product _ tys) k u = bvExtract i j u
  where j = sum (map typeSize (take k tys))
        i = j + typeSize (tys !! k) - 1
--getItem (Product _ tys) i e = bvExtract 

yicesExpr :: Expr -> YicesM Doc
yicesExpr (Literal Nothing (BoolLit b)) = text $ if b then "True" else "False"
yicesExpr (Literal (Just (ty,sz)) lit) = yicesLiteral ty lit
yicesExpr (DataCon _ (DC (Sum _ cs,i)) []) = text $ cs !! fromIntegral i
yicesExpr (DataCon t VecAppend [h,tl]) = applyN "bv-concat" $ mapM yicesExpr (h:elems tl)
  where elems (DataCon t VecAppend [h,tl]) = h:elems tl
        elems other = [other]
yicesExpr (DataCon t mod exprs) = applyN "bv-concat" $ mapM yicesExpr exprs
yicesExpr (Identifier i Nothing) = prefixed i
yicesExpr (Identifier i (Just (Indexed (ty,_,n)))) =
  getItem ty n $ prefixed i
yicesExpr (Identifier i (Just (DC _))) = prefixed i
yicesExpr (Identifier i mod) = prefixed i <+> text (Text.pack (show mod))
yicesExpr (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "CLaSH.Sized.Internal.Unsigned.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  = yicesLiteral (Unsigned (fromInteger n)) i
  | pNm == "CLaSH.Sized.Internal.Unsigned.resize#" 
  , [_,(Left e,ty,_)] <- bbInputs bbCtx
  , (_,oty) <- bbResult bbCtx
  , ts <- typeSize ty
  , os <- typeSize oty
  = if ts > os
    then applyN "bv-extract" (sequence [int (os - 1), "0", yicesExpr e])
    else if ts < os 
         then apply2 "bv-zero-extend" (yicesExpr e) (int (os - ts))
         else yicesExpr e
  | pNm == "CLaSH.Sized.Vector.init"
  , [(Left e,ty,_)] <- bbInputs bbCtx
  , (_,oty) <- bbResult bbCtx
  , os <- typeSize oty
  = applyN "bv-extract" (sequence [int (os - 1), "0",yicesExpr e])
  | pNm == "CLaSH.Sized.Vector.last"
  , [(Left e,ty,_)] <- bbInputs bbCtx
  , (_,oty) <- bbResult bbCtx
  , ts <- typeSize ty
  , os <- typeSize oty
  = applyN "bv-extract" (sequence [int (ts - 1), int (ts - os), yicesExpr e])
yicesExpr (BlackBoxE pNm _ _ Nothing bs bbCtx _) = do
  t <- renderBlackBox bs bbCtx
  string t

yicesExpr x = text (Text.pack (show x))

yicesLiteral :: HWType -> Literal -> YicesM Doc
yicesLiteral (s@(Sum name constructors)) (NumLit i) =
  text $ constructors !! fromInteger i
yicesLiteral (Unsigned sz) (NumLit i) = apply2 "mk-bv" (int sz) (int (fromInteger i))
yicesLiteral ty (NumLit i)    = text $ Text.pack $ show i
yicesLiteral ty (BitLit b)    = text $ Text.pack $ show b
yicesLiteral ty (BoolLit b)   = if b then "True" else "False"
yicesLiteral ty (VecLit lits) = undefined
yicesLiteral ty (StringLit s) = undefined

conjunction :: YicesM [Doc] -> YicesM Doc
conjunction = (>>= conj)
  where conj [] = "true"
        conj [x] = return x
        conj xs = applyN "and" (return xs)

yicesEquality :: HWType -> YicesM Doc -> YicesM Doc -> YicesM Doc
yicesEquality (Product _ tys) l r =
  applyN "and" $ forM (zip tys [1..]) $ \(ty,i) ->
    yicesEquality ty (apply2 "select" l (int i)) (apply2 "select" r (int i))
yicesEquality _ l r = apply2 "=" l r

yicesInst :: Declaration -> YicesM (Maybe Doc)
yicesInst (NetDecl _ _) = return Nothing
yicesInst (Assignment i e) =
  fmap Just $ apply "assert" $ apply2 "=" (prefixed i) (yicesExpr e)
yicesInst (CondAssignment i t e t2 cases) = do
    orElse <- yicesExpr defaultCase
    fmap Just $ apply "assert" $ apply2 "=" (prefixed i) $
      foldM buildCase orElse otherCases
  where
    (defaultCase,otherCases) = case partition (isNothing . fst) cases of
      ([],other) -> (snd $ last other, init other)
      ([d],other) -> (snd d, other)
      _ -> error "there should be at most one default case"
    buildCase el (Just lit,e2) = do
      l <- apply2 "=" (yicesExpr e) (yicesLiteral t2 lit)
      r <- yicesExpr e2
      applyN "ite" (return [l,r,el])
yicesInst nested@(BlackBoxD nm _ _ _ _ ctx) = fmap Just $ 
    comment ("begin" <+> nm') <$> body <$> comment ("end" <+> nm')        
  where
    body = case nm of
      "CLaSH.Sized.Vector.zipWith" -> do
        vsep $ forM [1 .. len 1] $ \i -> do
          px <- mkpx "zipWith" i
          a <- getItem (typ 1) (i-1) (yicesExpr (arg 1))
          b <- getItem (typ 2) (i-1) (yicesExpr (arg 2))
          o <- getItem (typo) (i-1) (yicesExpr out)
          withPrefix px $ (fun 0) [a,b] o
      "CLaSH.Sized.Vector.map" -> do
        vsep $ forM [1 .. len 1] $ \i -> do
          px <- mkpx "map" i
          a <- getItem (typ 1) (i - 1) (yicesExpr (arg 1))
          o <- getItem (typo) (i - 1) (yicesExpr out)
          withPrefix px $ (fun 0) [a] o
      "CLaSH.Sized.Vector.fold" -> do
        let pairs [] = []
            pairs [x] = [Left x]
            pairs (x:y:xs) = (Right (x,y)) : pairs xs
        let layer :: [Doc] -> YicesM [(Doc,Doc)]
            layer inputs = forM (pairs inputs) $ \case 
              Left x -> do
                f <- empty
                return (f,x)
              Right (x,y) -> do
                o <- tempVar                
                res <- prefixed o
                f <- (fmap fromJust (definition (NetDecl o typo))) <$> 
                     (withPrefix o $ (fun 0) [x,y] (res))
                return (f,res)
        let reduce :: [Doc] -> YicesM (Doc,Doc)
            reduce inputs = do
              res <- layer inputs
              case res of
                [(body,x)] -> return (body,x)
                other -> do 
                  doc <- vsep (return (map fst other))
                  (f,r) <- reduce (map snd other)
                  f' <- (return doc) <$> (return f)
                  return (f',r)
        top <- forM [1 .. len 1] $ \i -> do
          getItem (typ 1) (i - 1) (yicesExpr (arg 1))
        (body,x) <- reduce top
        (return body) <$> apply "assert" (apply2 "=" (yicesExpr out) (return x))
      _ ->
        text (Text.fromStrict nm) <$>
          indent 2 (text $ Text.pack (show ctx))
    nm' = text (Text.fromStrict nm)
    replaceArg temp subst = let (prefix,suffix') = Text.breakOn "~ARG[" temp
                                (_,suffix'') = Text.breakOn "]" suffix'
                                suffix = if (Text.length suffix'' > 0) then Text.tail suffix'' else ""
                            in  Text.append prefix (Text.append subst suffix)
    fun n i o = case (bbFunctions ctx) IntMap.! n of 
      (Right (InstDecl n ni _), ctx') -> do
        mods <- use otherModules
        case lookup n mods of
          Just c -> do            
            pipeThrough <- forM (bbInputs ctx') $ \case
              (Left e@(Identifier n Nothing), _, _) -> text n
            let ins = map fst (inputs c)
            let [out] = map fst (outputs c)
            let as = (out,o) : zip ins (pipeThrough ++ i)
            comment ("begin" <+> text ni) <$>
              --text (Text.pack $ show ctx) <$>
              vsep (forM as $ \(i,a) -> comment (" " <+> text i <> colon <+> return a)) <$>
              withAssignments as (genYicesBody c) <$>
              comment ("end" <+> text ni)
          Nothing ->
            comment $ "could not inst" <+> text ni      
      (Right (Assignment "~RESULT" (Identifier bb Nothing)), ctx') ->
        (apply "assert" (apply2 "=" (return o) (text $ foldl replaceArg bb (map (Text.pack . show) i))))
      (other,ctx') ->
        "could not inst" <$>
        text (Text.pack (show other)) <$>
        text (Text.pack (show ctx)) <$>
        text (Text.pack (show nested)) <$>        
        text (Text.pack (show ctx'))
    len n = case typ n of Vector n _ -> n
    typ n = case (bbInputs ctx) !! n of (_,t,_) -> t
    out = sync (fst (bbResult ctx))
    typo = snd (bbResult ctx)
    mkpx n i = ap mkBasicId (return (Text.append n (Text.append "_" (Text.pack (show i)))))
    arg n = case (bbInputs ctx) !! n of (e,_,_) -> sync e
    sync (Left e) = e
    sync (Right (e,_)) = e

  {-| nm == "CLaSH.Sized.Vector.zipWith" = fmap Just $ do
      let decl = case (bbFunctions ctx) IntMap.! 0 of
            (Right decl,_) -> decl
            other -> error "WHAT?"
      let (ae,Vector l at,_) = (bbInputs ctx) !! 1
      let (be,Vector _ bt,_) = (bbInputs ctx) !! 2
      let (out,Vector _ ot) = bbResult ctx
      let ae' = case ae of { Left e -> e; Right (e,_) -> e }
      let be' = case be of { Left e -> e; Right (e,_) -> e }
      let out' = case out of { Left e -> e; Right (e,_) -> e }
      a <- yicesExpr ae'
      b <- yicesExpr be'
      o <- yicesExpr out'
      comment "begin zipWith" <$>
      ( vsep $ forM [1..l] $ \i -> do
          let px = (Text.append "zipWith_" (Text.pack (show i)))
          a' <- (return a) <> (brackets (int (i-1)))
          b' <- (return b) <> (brackets (int (i-1)))
          o' <- (return o) <> (brackets (int (i-1)))
          as <- compAssignments c [a',b'] o'
          withAssignments as (withPrefix px (genYicesBody c)) <$>
            comment ("end" <+> text ni)
      ) <$>
      comment "end zipWith" -}
     
yicesInst (InstDecl n ni as) = fmap Just $ do
  mods <- use otherModules
  case lookup n mods of
    Just c ->
      comment ("begin" <+> text ni) <$>
      withPrefix ni (genYicesBody c) <$>
      comment ("end" <+> text ni)
    Nothing ->
      comment $ "could not inst" <+> text ni
yicesInst other = fmap Just $ text $ Text.pack $ show other

ports :: Component -> YicesM Doc
ports c =
    comment "inputs" <$> vsep ins <$>
    comment "hidden inputs" <$> vsep hins <$>
    comment "outputs" <$> vsep outs
  where
    ins   = forM (inputs c) $ \(n,t) -> apply "define" $ yicesSig n t
    hins  = forM (hiddenPorts c) $ \(n,t) -> apply "define" $ yicesSig n t
    outs  = forM (outputs c) $ \(n,t) -> apply "define" $ yicesSig n t

definitions :: Component -> YicesM Doc
definitions c =
    comment "wires" <$> vsep wires
  where
    wires = fmap catMaybes $ mapM definition (declarations c)

vectorIndex :: Text -> Int -> Text
vectorIndex n i = Text.append n (Text.append "[" (Text.append (Text.pack $ show i) "]"))

definition :: Declaration -> YicesM (Maybe Doc)
definition (NetDecl name ty) = fmap Just $ apply "define" $ yicesSig name ty
definition _ = return Nothing

---

define :: Identifier -> HWType -> Maybe Expr -> YicesM Doc
define n ty Nothing = apply "define" $ yicesSig n ty
define n ty (Just e) = apply2 "define" (yicesSig n ty) (yicesExpr e)

bvExtract :: Int -> Int -> YicesM Doc -> YicesM Doc
bvExtract i j u = applyN "bv-extract" (sequence [int i, int j, u])

---

nestedTypes :: HWType -> [HWType]
nestedTypes (Vector _ ty) = [ty]
nestedTypes (RTree _ ty) = [ty]
nestedTypes (Product _ tys) = tys
nestedTypes (SP _ xs) = concatMap snd xs
nestedTypes _ = []

---

mshow :: Show a => a -> YicesM Doc
mshow = text . Text.pack . show

dcolon :: YicesM Doc
dcolon = colon <> colon

apply :: Text -> YicesM Doc -> YicesM Doc
apply fname arg = applyN fname $ fmap (:[]) arg

apply2 :: Text -> YicesM Doc -> YicesM Doc -> YicesM Doc
apply2 fname arg1 arg2 = applyN fname $ sequence [arg1,arg2]

applyN :: Text -> YicesM [Doc] -> YicesM Doc
applyN fname args = parens $ text fname <+> align (sep args)

comment :: YicesM Doc -> YicesM Doc
comment = (<+>) semi