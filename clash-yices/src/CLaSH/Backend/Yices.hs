{-|
  Copyright   :  (C) 2015-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Generate Verilog for assorted Netlist datatypes
-}

{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}

module CLaSH.Backend.Yices (YicesState) where

--import qualified Control.Applicative                  as A
import           Control.Lens                         ((+=),(-=),(.=),(%=),makeLenses, use)
import           Control.Monad.State                  (State,forM_,forM)
import           Data.List (nub)
import qualified Data.HashSet                         as HashSet
import           Data.Maybe                           (mapMaybe,catMaybes,maybeToList,fromMaybe)
import qualified Data.IntMap.Lazy as IM
import           Data.Text.Lazy                       (pack, unpack)
import qualified Data.Text.Lazy                       as Text
import qualified Data.Text.Lazy.Read                  as Text
import           Prelude                              hiding ((<$>))
import           Text.PrettyPrint.Leijen.Text.Monadic

import           CLaSH.Annotations.Primitive          (HDL (..))
import           CLaSH.Backend
import           CLaSH.Driver.Types                   (SrcSpan, noSrcSpan)
import           CLaSH.Netlist.BlackBox.Types         (HdlSyn)
import           CLaSH.Netlist.BlackBox.Util          (extractLiterals, renderBlackBox)
import           CLaSH.Netlist.Id                     (mkBasicId')
import           CLaSH.Netlist.Types                  hiding (_intWidth, intWidth)
import           CLaSH.Netlist.Util                   hiding (mkBasicId)
import           CLaSH.Util                           (curLoc, (<:>))


#ifdef CABAL
import qualified Paths_clash_yices
#else
import qualified System.FilePath
#endif

-- | State for the 'CLaSH.Backend.Verilog.YicesM' monad:
data YicesState =
  YicesState
    { _genDepth      :: Int -- ^ Depth of current generative block
    , _idSeen        :: [Identifier]
    , _currentInputs :: [Identifier]
    , _srcSpan       :: SrcSpan    
    , _packages      :: [Text.Text]
    , _types         :: [(Identifier,HWType)]
    , _imports       :: [Text.Text]
    , _functions     :: [State YicesState Doc]
    , _defs          :: [(Identifier,State YicesState Doc)]
    , _includes      :: [(String,Doc)]
    , _intWidth      :: Int -- ^ Int/Word/Integer bit-width
    , _hdlsyn        :: HdlSyn
    }

makeLenses ''YicesState

instance Backend YicesState where
  initBackend     = YicesState 0 [] [] noSrcSpan [] [] [] [] [] []
  hdlKind         = const Yices
#ifdef CABAL
  primDir         = const (Paths_clash_yices.getDataFileName "primitives")
#else
  primDir _       = return ("clash-yices" System.FilePath.</> "primitives")
#endif
  extractTypes    = const HashSet.empty
  name            = const "yices"
  extension       = const ".ys"  
  genHDL          = const genYices
  mkTyPackage _ _ = return []
  hdlType         = yicesType
  hdlTypeErrValue = yicesTypeErrValue
  hdlTypeMark     = yicesTypeMark
  hdlSig t ty     = sigDecl (text t) ty
  genStmt True    = do cnt <- use genDepth
                       genDepth += 1
                       if cnt > 0
                          then empty
                          else "generate"
  genStmt False   = do genDepth -= 1
                       cnt <- use genDepth
                       if cnt > 0
                          then empty
                          else "endgenerate"
  inst            = inst'
  expr            = const expr'
  iwWidth         = use intWidth
  toBV _          = text
  fromBV _        = text
  hdlSyn          = use hdlsyn
  mkBasicId       = return (filterReserved . mkBasicId' True)
  setModName _    = id
  setSrcSpan      = (srcSpan .=)
  getSrcSpan      = use srcSpan

type YicesM a = State YicesState a

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

filterReserved :: Identifier -> Identifier
filterReserved s = if s `elem` reservedWords
  then s `Text.append` "_r"
  else s

-- | Generate Yices for a Netlist component
genYices :: SrcSpan -> Component -> YicesM ((String,Doc),[(String,Doc)])
genYices sp c = do
    setSrcSpan sp
    v    <- yices
    _    <- use packages
    incs <- use includes    
    return ((unpack cName,v),incs)
  where
    cName   = componentName c
    yices =       
      "; Automatically generated Yices Bitvector Logic" <$$>
      module_ c

module_ :: Component -> YicesM Doc
module_ c = do
    addSeen c
    decls (declarations c)
    forM_ (inputs c ++ hiddenPorts c) $ \(i,_) ->
      currentInputs %= (i:)
    m <- ";decls" <$>
         vsep (sequence [ semi <+> (text $ pack $ show d) | d <- declarations c ]) <$>
         vsep (use imports >>= sequence . map (\x -> parens $ "include" <+> dquotes (text x <> ".ys"))) <$>
         vsep (use functions >>= sequence) <$>
         parens (        
          "define" <+> text (componentName c) <+> "::" <+> signature <+> lparen <$>
          indent 2 ("lambda" <+> args <+> lparen <> "bv-concat" <$>
            indent 2 (
              --decls (declarations c) <$>
              --insts (declarations c) <$>
              --(text $ pack $ show ts) <$>
              vsep (sequence [ align (getDef i)| (i,_) <- outputs c])
            ) <$> rparen
          ) <$> rparen)
    return m
  where
    {-ports = sequence
          $ [ encodingNote hwty <$> text i | (i,hwty) <- inputs c ] ++
            [ encodingNote hwty <$> text i | (i,hwty) <- hiddenPorts c] ++
            [ encodingNote hwty <$> text i | (i,hwty) <- outputs c]-}
    signature = parens $ "->" <+> inputTypes <+> outputTypes
    inputTypes = case (inputs c ++ hiddenPorts c) of
      [] -> empty
      p  -> hcat (punctuate space (sequence [ yicesType ty | (_,ty) <- p ]))
    outputTypes = case (outputs c) of
      [] -> empty
      p  -> hcat (punctuate space (sequence [ yicesType ty | (_,ty) <- p ]))      
    args = parens $ align $ inputPorts -- <$> outputPorts    
    inputPorts = case (inputs c ++ hiddenPorts c) of
      [] -> empty
      p  -> vcat (sequence [ sigDecl (text i) ty | (i,ty) <- p ])
    --outputPorts = case (outputs c) of
    --  [] -> empty
    --  p  -> vcat (sequence [ sigDecl (text i) ty | (i,ty) <- p ])
    --outConstraints = vcat (sequence )

addSeen :: Component -> YicesM ()
addSeen c = do
  let iport = map fst $ inputs c
      hport = map fst $ hiddenPorts c
      oport = map fst $ outputs c
      nets  = mapMaybe (\case {NetDecl i _ -> Just i; _ -> Nothing}) $ declarations c
  idSeen .= concat [iport,hport,oport,nets]

--mkUniqueId :: Identifier -> YicesM Identifier
--mkUniqueId i = do
--  mkId <- mkBasicId
--  seen <- use idSeen
--  let i' = mkId i
--  case i `elem` seen of
--    True  -> go mkId seen i' 0
--    False -> do idSeen %= (i':)
--                return i'
--  where
--    go :: (Identifier -> Identifier) -> [Identifier] -> Identifier
--       -> Int -> YicesM Identifier
--    go mkId seen i' n = do
--      let i'' = mkId (Text.append i' (Text.pack ('_':show n)))
--      case i'' `elem` seen of
--        True  -> go mkId seen i' (n+1)
--        False -> do idSeen %= (i'':)
--                    return i''
--
yicesType :: HWType -> YicesM Doc
yicesType t = case t of
  Signed n -> parens ("bitvector" <+> int n)
  Clock {} -> empty
  Reset {} -> empty
  _        -> parens ("bitvector" <+> int (typeSize t))

sigDecl :: YicesM Doc -> HWType -> YicesM Doc
sigDecl d t = d <+> "::" <+> yicesType t

-- | Convert a Netlist HWType to the root of a Verilog type
yicesTypeMark :: HWType -> YicesM Doc
yicesTypeMark = const empty

-- | Convert a Netlist HWType to an error Yices value for that type
yicesTypeErrValue :: HWType -> YicesM Doc
yicesTypeErrValue ty = braces (int (typeSize ty) <+> braces "1'bx")

inst' :: Declaration -> YicesM (Maybe Doc)
inst' (InstDecl nm _ ps) = do
    imports %= (nm:)
    fmap Just $ parens $ text nm <+> text (pack (show sig))
  where
    ins = filter (\(_,x,_,_) -> case x of {In -> True; _ -> False}) ps
    sig = map (\(_,_,ty,e) -> (e,ty)) ins
inst' x = fmap Just $ text $ pack $ show x

decls :: [Declaration] -> YicesM ()
decls = mapM_ decl

decl :: Declaration -> YicesM ()
decl (NetDecl id_ ty) = do
  ts <- use types  
  types .= (id_,ty) : ts
  return ()

decl (Assignment id_ e) = do
  ins <- getInputs e
  ot <- getType id_
  addFunction id_ ins (fromMaybe Void ot) $ withInputs (map fst ins) (expr' e)
  addDefinition id_ $ expr' e  

decl (CondAssignment id_ _ scrut _ [(Just (BoolLit b), l),(_,r)]) =  
  addDefinition id_ $
    parens ("ite" <+> align (parens ("bit" <+> expr' scrut <+> "0") <$> expr' t <$> expr' f))  
 where (t,f) = if b then (l,r) else (r,l)

--decl (InstDecl nm _ _) =
--  imports %= (nm:)

decl (BlackBoxD _ _ _ _ bs bbCtx) = do
  let (se,ty) = bbResult bbCtx
  let mkty (_,Void,_) = Nothing
      mkty (i,t,_) = case i of 
        Left (Identifier x _) -> Just (x,t)
        _ -> Nothing
  let ins = catMaybes $ map mkty $ bbInputs bbCtx
  let id_ = case se of
        Left (Identifier x _) -> Just x
        _ -> Nothing
  let extractInst ((Right (InstDecl nm _ _)),_) = Just nm
      extractInst _ = Nothing
  let insts = catMaybes $ map extractInst $ IM.elems (bbFunctions bbCtx)
  let substFun (se',tx,l) i = case IM.lookup i (bbFunctions bbCtx) of
        Just (Right (InstDecl nm _ _),_) -> (Left $ Identifier nm Nothing,tx,l)
        _ -> (se',tx,l)
  let bbCtx' = bbCtx {
        bbInputs = zipWith substFun (bbInputs bbCtx) [0..]
      }
  imports %= (nub . (insts ++))
  forM_ id_ $ \i -> do
    addFunction i ins ty $ withInputs insts $
      renderBlackBox bs bbCtx' >>= postProcessBlackbox
    addDefinition i $ 
      parens (text i <+> (hcat $ punctuate space $ forM ins (\(i',_) -> getDef i')))

decl (InstDecl _ _ _) = return ()

decl d = error $ show d

getType :: Identifier -> YicesM (Maybe HWType)
getType id_ = do
  ts <- use types
  return $ lookup id_ ts

getInputs :: Expr -> YicesM [(Identifier,HWType)]
getInputs (Identifier id_ _) = do
  t <- getType id_
  return $ maybeToList $ fmap (id_,) t
getInputs (DataCon _ _ es) = do
  ess <- mapM getInputs es
  return $ concat ess
getInputs (BlackBoxE _ _ _ _ _ ctx _) = do
  let es = bbInputs ctx
  let f (Left e,_,_) = getInputs e
      f (Right (e,_),_,_) = getInputs e      
  ess <- mapM f es
  return $ concat ess
getInputs _ = return []

--renderSyncExpr :: SyncExpr -> YicesM Doc
--renderSyncExpr (Left (Identifier x _)) = getDef x
--renderSyncExpr _ = error "sync"

addFunction :: Identifier -> [(Identifier,HWType)] -> HWType -> YicesM Doc -> YicesM ()
addFunction n sig rt f = functions %= (t:)
  where tsig = parens $ "->" <+> hcat (punctuate space (mapM yicesType ((map snd sig) ++ [rt])))
        lambda = "lambda" <+> parens (align (vcat $ forM sig $ \(i,t_) -> text i <+> "::" <+> yicesType t_))
        body = withInputs (map fst sig) f
        t = parens $ "define" <+> text n <+> "::" <+> tsig <+> lparen <$> indent 2 (lambda </> indent 2( body )) <$> rparen

addDefinition :: Identifier -> YicesM Doc -> YicesM ()
addDefinition id_ f = do
  ds <- use defs  
  defs .= (id_, f) : ds

withInputs :: [Identifier] -> YicesM a -> YicesM a
withInputs is x = do
  before <- use currentInputs
  currentInputs .= is ++ before
  res <- x 
  currentInputs .= before
  return res

getDef :: Identifier -> YicesM Doc
getDef id_ = do 
  u <- use currentInputs
  if elem id_ u
  then text id_
  else do
    ds <- use defs
    case lookup id_ ds of
      Nothing -> "ERROR:" <+> text id_
      Just f  -> withInputs [id_] f
  
expr' :: Expr -> YicesM Doc
expr' (Literal sizeM lit) = exprLit sizeM lit

expr' (Identifier id_ Nothing) = getDef id_

expr' (Identifier id_ (Just (Indexed (ty@(SP _ args),dcI,fI)))) =
    getDef id_ <> brackets (int start <> colon <> int end)
  where
    argTys   = snd $ args !! dcI
    argTy    = argTys !! fI
    argSize  = typeSize argTy
    other    = otherSize argTys (fI-1)
    start    = typeSize ty - 1 - conSize ty - other
    end      = start - argSize + 1

expr' (Identifier id_ (Just (Indexed (ty@(Product _ argTys),_,fI)))) =
    parens $ 
      case argTy of
        Bool -> "bit" <+> getDef id_ <+> int start
        _    -> "bv-extract" <+> int start <+> int end <+> getDef id_
  where
    argTy   = argTys !! fI
    argSize = typeSize argTy
    otherSz = otherSize argTys (fI - 1)
    start   = typeSize ty - 1 - otherSz
    end     = start - argSize + 1

expr' (Identifier id_ (Just (Indexed (ty@(Vector _ argTy),1,1)))) =
    getDef id_ <> brackets (int start <> colon <> int end)
  where
    argSize = typeSize argTy
    start   = typeSize ty - 1
    end     = start - argSize + 1

expr' (Identifier id_ (Just (Indexed (ty@(Vector _ argTy),1,2)))) =
    getDef id_ <> brackets (int start <> colon <> int 0)
  where
    argSize = typeSize argTy
    start   = typeSize ty - argSize - 1

expr' (Identifier id_ (Just (Indexed ((RTree 0 _),0,1)))) = getDef id_

expr' (Identifier id_ (Just (Indexed (ty@(RTree _ _),1,1)))) =
    getDef id_ <> brackets (int start <> colon <> int end)
  where
    start   = typeSize ty - 1
    end     = typeSize ty `div` 2

expr' (Identifier id_ (Just (Indexed (ty@(RTree _ _),1,2)))) =
    getDef id_ <> brackets (int start <> colon <> int 0)
  where
    start   = (typeSize ty `div` 2) - 1

-- This is a HACK for CLaSH.Driver.TopWrapper.mkOutput
-- Vector's don't have a 10'th constructor, this is just so that we can
-- recognize the particular case
expr' (Identifier id_ (Just (Indexed (ty@(Vector _ argTy),10,fI)))) =
    getDef id_ <> brackets (int start <> colon <> int end)
  where
    argSize = typeSize argTy
    start   = typeSize ty - (fI * argSize) - 1
    end     = start - argSize + 1

-- This is a HACK for CLaSH.Driver.TopWrapper.mkOutput
-- RTree's don't have a 10'th constructor, this is just so that we can
-- recognize the particular case
expr' (Identifier id_ (Just (Indexed (ty@(RTree _ argTy),10,fI)))) =
    getDef id_ <> brackets (int start <> colon <> int end)
  where
    argSize = typeSize argTy
    start   = typeSize ty - (fI * argSize) - 1
    end     = start - argSize + 1

expr' (Identifier id_ (Just (DC (ty@(SP _ _),_)))) = getDef id_ <> brackets (int start <> colon <> int end)
  where
    start = typeSize ty - 1
    end   = typeSize ty - conSize ty

expr' (Identifier id_ (Just _))                      = getDef id_

expr' (DataCon _ (DC (Void, -1)) [e]) = expr' e

expr' (DataCon ty@(Vector 0 _) _ _) = yicesTypeErrValue ty

expr' (DataCon (Vector 1 _) _ [e]) = expr' e
expr' e@(DataCon (Vector _ _) _ es@[_,_]) =
  case vectorChain e of
    Just es' -> bvConcat (mapM (expr') es')
    Nothing  -> bvConcat (mapM (expr') es)

expr' (DataCon (RTree 0 _) _ [e]) = expr' e
expr' e@(DataCon (RTree _ _) _ es@[_,_]) =
  case rtreeChain e of
    Just es' -> bvConcat (mapM (expr') es')
    Nothing  -> bvConcat (mapM (expr') es)

expr' (DataCon ty@(SP _ args) (DC (_,i)) es) = assignExpr
  where
    argTys     = snd $ args !! i
    dcSize     = conSize ty + sum (map typeSize argTys)
    dcExpr     = expr' (dcToExpr ty i)
    argExprs   = map (expr') es
    extraArg   = case typeSize ty - dcSize of
                   0 -> []
                   n -> ["0b" <> bits (replicate n U)]
    assignExpr = parens $ "bv-concat" <+> (hcat $ punctuate space $ sequence (dcExpr:argExprs ++ extraArg))

expr' (DataCon ty@(Sum _ _) (DC (_,i)) []) = int (typeSize ty) <> "'d" <> int i

expr' (DataCon (Product _ tys) _ es) = bvConcat $ mapM z (zip tys es)  
  where z (_,e) = expr' e

expr' (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "CLaSH.Sized.Internal.Signed.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  = exprLit (Just (Signed (fromInteger n),fromInteger n)) i

expr' (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "CLaSH.Sized.Internal.Unsigned.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  = exprLit (Just (Unsigned (fromInteger n),fromInteger n)) i

expr' (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "CLaSH.Sized.Internal.BitVector.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  = exprLit (Just (BitVector (fromInteger n),fromInteger n)) i

expr' (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "CLaSH.Sized.Internal.Index.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  = exprLit (Just (Index (fromInteger n),fromInteger n)) i

expr' (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "CLaSH.Sized.Internal.Unsigned.resize#"
  , Context (_,oty) [(_,_,_),(Left e,ity,_)] _ _ <- bbCtx
  = if typeSize oty > typeSize ity 
    then parens ("bv-zero-extend" <+> expr' e <+> text (pack $ show $ typeSize oty))
    else parens ("bv-extract" <+> text (pack $ show $ (typeSize oty) - 1) <+> "0" <+> expr' e)

expr' (BlackBoxE _ _ _ Nothing bs bbCtx _) = do
  renderBlackBox bs bbCtx >>= procExpr  
  
expr' e = error $ $(curLoc) ++ (show e) -- empty

otherSize :: [HWType] -> Int -> Int
otherSize _ n | n < 0 = 0
otherSize []     _    = 0
otherSize (a:as) n    = typeSize a + otherSize as (n-1)

vectorChain :: Expr -> Maybe [Expr]
vectorChain (DataCon (Vector 0 _) _ _)       = Just []
vectorChain (DataCon (Vector 1 _) _ [e])     = Just [e]
vectorChain (DataCon (Vector _ _) _ [e1,e2]) = Just e1 <:> vectorChain e2
vectorChain _                                = Nothing

rtreeChain :: Expr -> Maybe [Expr]
rtreeChain (DataCon (RTree 0 _) _ [e])     = Just [e]
rtreeChain (DataCon (RTree _ _) _ [e1,e2]) = Just e1 <:> rtreeChain e2
rtreeChain _                               = Nothing

exprLit :: Maybe (HWType,Size) -> Literal -> YicesM Doc
exprLit Nothing (NumLit i) = integer i

exprLit (Just (hty,sz)) (NumLit i) = case hty of
  Unsigned _ -> bvlit
  Index _ -> int (typeSize hty) <> "'d" <> integer i
  Signed _
   | i < 0     -> parens $ "bv-neg" <+> bvlit
   | otherwise -> bvlit
  _ -> "0b" <> blit
  where
    bvlit = parens $ "mk-bv" <+> int sz <+> integer (abs i)
    blit = bits (toBits sz i)
exprLit _             (BoolLit t)   = if t then "true" else "false"
exprLit _             (BitLit b)    = "0b" <> bit_char b
exprLit _             (StringLit s) = text . pack $ show s
exprLit _             l             = error $ $(curLoc) ++ "exprLit: " ++ show l

toBits :: Integral a => Int -> a -> [Bit]
toBits size val = map (\x -> if odd x then H else L)
                $ reverse
                $ take size
                $ map (`mod` 2)
                $ iterate (`div` 2) val

bits :: [Bit] -> YicesM Doc
bits = hcat . mapM bit_char

bit_char :: Bit -> YicesM Doc
bit_char H = char '1'
bit_char L = char '0'
bit_char U = char 'x'
bit_char Z = char 'z'

dcToExpr :: HWType -> Int -> Expr
dcToExpr ty i = Literal (Just (ty,conSize ty)) (NumLit (toInteger i))

bvConcat :: YicesM [Doc] -> YicesM Doc
bvConcat = parens . ("bv-concat" <+>) . align . vsep

postProcessBlackbox :: Text.Text -> YicesM Doc
postProcessBlackbox r = procLines (Text.lines $ r) []
  where procLines :: [Text.Text] -> [(Text.Text,Text.Text)] -> YicesM Doc
        procLines [] _ = empty
        procLines (l:ls) ds = case Text.words l of
          ["$for",v,"<-",sm,s,"..",e,em] -> do
            let (x,y) = span ((/= ["$endfor"]) . Text.words) ls
            let mod' "[" = 0
                mod' "]" = 0
                mod' "(" = 1
                mod' ")" = -1
                mod' x' = error $ "not a range delimiter: " ++ show x'
            let pint :: Text.Text -> Int
                pint = either (const 0) (fst) . Text.decimal
            let s' = pint s + mod' sm
            let e' = pint e + mod' em
            let uf = vcat $ forM [s'..e'] $ \i -> do
                       let ds' = (v,pack (show i)):ds
                       (procLines x ds')
            uf <$> procLines (tail y) ds
          "$let":i:"=":d -> do
            let ds' = (i,Text.unwords d):ds
            procLines ls ds'
          _ -> procExpr' l ds <$> procLines ls ds
        procExpr' :: Text.Text -> [(Text.Text,Text.Text)] -> YicesM Doc
        procExpr' l = 
          procExpr . foldl (\l' (n,d) -> Text.replace (Text.cons '$' n) d l') l

procExpr :: Text.Text -> YicesM Doc        
procExpr t | Text.length t == 0 = empty
procExpr t = do
  let (b,a) = Text.span (/= '$') t
  if Text.length a > 0
  then do
    let (e,a') = Text.span (/= '$') (Text.tail a)
    if Text.length a' > 0
    then text b <> text (pack (show (eval e))) <> procExpr (Text.tail a')
    else text b -- <> procExpr' a'
  else text b

evalLiteral :: Text.Text -> Int 
evalLiteral t = case Text.decimal t of
  Right (i,_) -> i
  _ -> error $ "no int: " ++ show t

eval :: Text.Text -> Int
eval = (foldl1 (+) ) . (map eval') . (Text.splitOn "+")
  where eval' = foldl1 (-) . map eval'' . Text.splitOn "-"
        eval'' = foldl1 (*) . map evalLiteral . Text.splitOn "*"
  
--punctuate' :: Monad m => m Doc -> m [Doc] -> m Doc
--punctuate' s d = vcat (punctuate s d) <> s


{-encodingNote :: HWType -> YicesM Doc
encodingNote (Clock _ _) = "// clock"
encodingNote (Reset _ _) = "// asynchronous reset: active low"
encodingNote _           = empty-}
