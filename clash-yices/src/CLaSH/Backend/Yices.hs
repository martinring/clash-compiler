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

import           Control.Lens                         hiding (Indexed)

import           Text.PrettyPrint.Leijen.Text.Monadic

import           Control.Monad
import           Control.Monad.State                  (State)

import           Data.List                            (intersect,partition)
import           Data.Maybe                           (catMaybes)
import           Data.HashSet                         (HashSet)
import qualified Data.HashSet                         as HashSet
import           Data.Text.Lazy                       (Text)
import qualified Data.Text.Lazy                       as Text

import           CLaSH.Annotations.Primitive          (HDL (..))
import           CLaSH.Backend
import           CLaSH.Driver.Types                   (SrcSpan, noSrcSpan)
import           CLaSH.Netlist.BlackBox.Types         (HdlSyn (..))
import           CLaSH.Netlist.BlackBox.Util          (renderBlackBox,extractLiterals)
import           CLaSH.Netlist.Id                     (mkBasicId')
import           CLaSH.Netlist.Types                  hiding (_intWidth, intWidth)

#ifdef CABAL
import qualified Paths_clash_yices
#else
import qualified System.FilePath
#endif

-- | State for the 'CLaSH.Backend.Verilog.YicesM' monad:
data YicesState =
  YicesState
    { _typesSeen     :: HashSet HWType
    , _typeNames     :: [(HWType,String)]
    , _srcSpan       :: SrcSpan
    , _intWidth      :: Int -- ^ Int/Word/Integer bit-width
    , _currentModule :: ModName
    }

makeLenses ''YicesState

type YicesM a = State YicesState a

instance Backend YicesState where
  -- | Initial state for state monad
  initBackend :: Int -> HdlSyn -> YicesState
  initBackend iw _ = YicesState HashSet.empty [] noSrcSpan iw ""
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

filterReserved :: Identifier -> Identifier
filterReserved s = if s `elem` reservedWords
  then s `Text.append` "_r"
  else s

genYices :: String -> SrcSpan -> Component -> YicesM ((String, Doc),[(String,Doc)])
genYices nm sp c = do
  setSrcSpan sp
  currentModule .= nm
  let cname = componentName c

  let defs = definitions c
  let wiring = vsep $ fmap catMaybes $ mapM yicesInst (declarations c)
  let tmn = Text.pack nm `Text.append` "_types.ys"
  text <-
    apply "include" (dquotes $ text tmn) <$>
    defs <$>
    comment "network" <$>
    wiring
  return ((Text.unpack cname,text),[])

yicesType :: HWType -> YicesM Doc
yicesType Void = "<void>"
yicesType String = "<string>"
yicesType Bool = "bool"
yicesType (BitVector sz) = apply "bitvector" $ int sz
yicesType (Index _) = "<index>"
yicesType (Signed sz) = apply "bitvector" $ int sz
yicesType (Unsigned sz) = apply "bitvector" $ int sz
yicesType (Vector sz ty) = undefined
yicesType (RTree sz ty) = undefined
yicesType s@(Sum name constructors) = do
  typesSeen %= HashSet.insert s
  text name
yicesType p@(Product name tys)
  | Text.isPrefixOf "GHC.Tuple" name = applyN "tuple" $ mapM yicesType tys
  | otherwise = do
      typesSeen %= HashSet.insert p
      text name
yicesType (SP name cs) = undefined
yicesType (Clock name period) = "<clock>"
yicesType (Reset name period) = "<reset>"

yicesTypeDef :: HWType -> YicesM Doc
yicesTypeDef (Sum name constructors) =
  applyN "define-type" $ do
    name <- text name
    def <- applyN "scalar" $ mapM text constructors
    return [name,def]
yicesTypeDef (Product name tys) =
  applyN "define-type" $ do
    name <- text name
    def <- applyN "tuple" $ mapM yicesType tys
    return [name,def]
yicesTypeDef _ = "def"

sortTypes :: [HWType] -> [HWType]
sortTypes tys
  | length tys < 2 = tys
  | otherwise = ok ++ sortTypes rs
    where (ok,rs) = partition (null . intersect tys . nestedTypes) tys

yicesTypesPackage :: String -> [HWType] -> YicesM [(String, Doc)]
yicesTypesPackage name tys = do
  x <- vsep $ mapM yicesTypeDef (sortTypes tys)
  return [(name ++ "_types",x)]

yicesSig :: Text -> HWType -> YicesM Doc
yicesSig name ty = text name <+> dcolon <+> yicesType ty

yicesExpr :: Expr -> YicesM Doc
yicesExpr (Literal (Just (ty,sz)) lit) = yicesLiteral ty lit
yicesExpr (DataCon _ (DC (Sum _ cs,i)) []) = text $ cs !! fromIntegral i
yicesExpr (DataCon t mod exprs) = applyN "mk-tuple" $ mapM yicesExpr exprs
yicesExpr (Identifier i Nothing) = text i
yicesExpr (Identifier i (Just (Indexed (Product name tys,x,n)))) = apply "select" $ text i <+> int (n + 1)
yicesExpr (Identifier i (Just (DC _))) = text i
yicesExpr (Identifier i mod) = text i <+> text (Text.pack (show mod))
yicesExpr (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "CLaSH.Sized.Internal.Unsigned.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  = yicesLiteral (Unsigned (fromInteger n)) i
yicesExpr (BlackBoxE _ _ _ Nothing bs bbCtx _) = do
  t <- renderBlackBox bs bbCtx
  string t
yicesExpr x = text (Text.pack (show x))

yicesLiteral :: HWType -> Literal -> YicesM Doc
yicesLiteral (s@(Sum name constructors)) (NumLit i) =
  text $ constructors !! fromInteger i
yicesLiteral (Unsigned sz) (NumLit i) = apply "mk-bv" $ int sz <+> int (fromInteger i)
yicesLiteral ty (NumLit i)    = text $ Text.pack $ show i
yicesLiteral ty (BitLit b)    = text $ Text.pack $ show b
yicesLiteral ty (BoolLit b)   = if b then "true" else "false"
yicesLiteral ty (VecLit lits) = undefined
yicesLiteral ty (StringLit s) = undefined

yicesInst :: Declaration -> YicesM (Maybe Doc)
yicesInst (Assignment i e) =
  fmap Just $ apply "assert" $ apply "=" $ text i <+> yicesExpr e
yicesInst (CondAssignment i t e t2 cases) = fmap Just $ vsep $ mapM buildCase cases
  where buildCase (Just lit,e2) = do
          let l = apply "=" $ yicesExpr e <+> yicesLiteral t2 lit
          let r = apply "=" $ text i <+> yicesExpr e2
          apply "assert" $ apply "=>" $ l <+> r
yicesInst _ = return Nothing

definitions :: Component -> YicesM Doc
definitions c =
    comment "inputs" <$> vsep ins <$>
    comment "hidden inputs" <$> vsep hins <$>
    comment "outputs" <$> vsep outs <$>
    comment "wires" <$> vsep wires
  where
    ins  = forM (inputs c) $ \(n,t) -> apply "define" $ yicesSig n t
    hins = forM (hiddenPorts c) $ \(n,t) -> apply "define" $ yicesSig n t
    outs = forM (outputs c) $ \(n,t) -> apply "define" $ yicesSig n t
    wires = fmap catMaybes $ forM (declarations c) definition

definition :: Declaration -> YicesM (Maybe Doc)
definition (NetDecl name ty) = fmap Just $ do
  apply "define" $ yicesSig name ty
definition _ = return Nothing

---

nestedTypes :: HWType -> [HWType]
nestedTypes (Vector _ ty) = [ty]
nestedTypes (RTree _ ty) = [ty]
nestedTypes (Product _ tys) = tys
nestedTypes (SP _ xs) = concatMap snd xs
nestedTypes _ = []

---

dcolon :: YicesM Doc
dcolon = colon <> colon

apply :: Text -> YicesM Doc -> YicesM Doc
apply fname arg = parens $ text fname <+> arg

applyN :: Text -> YicesM [Doc] -> YicesM Doc
applyN fname args = parens $ text fname <+> align (sep args)

comment :: Text -> YicesM Doc
comment = (<+>) semi . text