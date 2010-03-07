import BinaryDerive

import Language.C.Data.Ident
import Language.C.Data.Node
import Language.C.Syntax.AST
import Language.C.Syntax.Constants


imports =
  unlines
  [ "module Language.C.Syntax.BinaryInstances where"
  , ""
  , "import Data.Binary"
  , "import Language.C.Syntax.AST"
  , "import Language.C.Syntax.Constants"
  , "import Language.C.Syntax.Ops"
  , "import Language.C.Syntax.ManualBinaryInstances"
  ]

main = do
  putStrLn imports
  -- Language.C.Data.Ident
  --deriveM (undefined :: Ident)
  -- Language.C.Data.Node
  --deriveM (undefined :: NodeInfo)
  -- Language.C.Syntax.AST
  deriveM (undefined :: CTranslUnit) >> putStrLn ""
  deriveM (undefined :: CExtDecl) >> putStrLn ""
  deriveM (undefined :: CFunDef) >> putStrLn ""
  deriveM (undefined :: CDecl) >> putStrLn ""
  deriveM (undefined :: CStructUnion) >> putStrLn ""
  deriveM (undefined :: CStructTag) >> putStrLn ""
  deriveM (undefined :: CEnum) >> putStrLn ""
  deriveM (undefined :: CDeclSpec) >> putStrLn ""
  deriveM (undefined :: CStorageSpec) >> putStrLn ""
  deriveM (undefined :: CTypeSpec) >> putStrLn ""
  deriveM (undefined :: CTypeQual) >> putStrLn ""
  deriveM (undefined :: CAttr) >> putStrLn ""
  deriveM (undefined :: CDeclr) >> putStrLn ""
  deriveM (undefined :: CDerivedDeclr) >> putStrLn ""
  deriveM (undefined :: CArrSize) >> putStrLn ""
  deriveM (undefined :: CInit) >> putStrLn ""
  deriveM (undefined :: CDesignator) >> putStrLn ""
  deriveM (undefined :: CStat) >> putStrLn ""
  deriveM (undefined :: CBlockItem) >> putStrLn ""
  deriveM (undefined :: CAsmStmt) >> putStrLn ""
  deriveM (undefined :: CAsmOperand) >> putStrLn ""
  deriveM (undefined :: CExpr) >> putStrLn ""
  deriveM (undefined :: CAssignOp) >> putStrLn ""
  deriveM (undefined :: CBinaryOp) >> putStrLn ""
  deriveM (undefined :: CUnaryOp) >> putStrLn ""
  deriveM (undefined :: CBuiltin) >> putStrLn ""
  deriveM (undefined :: CConst) >> putStrLn ""
  deriveM (undefined :: CStrLit) >> putStrLn ""
  -- Language.C.Syntax.Contants
  --deriveM (undefined :: CIntFlag) >> putStrLn ""
  --deriveM (undefined :: CIntRepr) >> putStrLn ""
  --deriveM (undefined :: Flags CIntFlag) >> putStrLn ""
  --deriveM (undefined :: CInteger) >> putStrLn ""
  --deriveM (undefined :: CChar) >> putStrLn ""
  --deriveM (undefined :: CFloat) >> putStrLn ""
  --deriveM (undefined :: CString) >> putStrLn ""
