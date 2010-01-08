import BinaryDerive

import Language.C.Data.Ident
import Language.C.Data.Node
import Language.C.Syntax.AST
import Language.C.Syntax.Constants

main = do
  -- Language.C.Data.Ident
  --deriveM (undefined :: Ident)
  -- Language.C.Data.Node
  --deriveM (undefined :: NodeInfo)
  -- Language.C.Syntax.AST
  deriveM (undefined :: CTranslUnit)
  deriveM (undefined :: CExtDecl)
  deriveM (undefined :: CFunDef)
  deriveM (undefined :: CDecl)
  deriveM (undefined :: CStructUnion)
  deriveM (undefined :: CStructTag)
  deriveM (undefined :: CEnum)
  deriveM (undefined :: CDeclSpec)
  deriveM (undefined :: CStorageSpec)
  deriveM (undefined :: CTypeSpec)
  deriveM (undefined :: CTypeQual)
  deriveM (undefined :: CAttr)
  deriveM (undefined :: CDeclr)
  deriveM (undefined :: CDerivedDeclr)
  deriveM (undefined :: CArrSize)
  deriveM (undefined :: CInit)
  deriveM (undefined :: CDesignator)
  deriveM (undefined :: CStat)
  deriveM (undefined :: CBlockItem)
  deriveM (undefined :: CAsmStmt)
  deriveM (undefined :: CAsmOperand)
  deriveM (undefined :: CExpr)
  deriveM (undefined :: CAssignOp)
  deriveM (undefined :: CBinaryOp)
  deriveM (undefined :: CUnaryOp)
  deriveM (undefined :: CBuiltin)
  deriveM (undefined :: CConst)
  deriveM (undefined :: CStrLit)
  -- Language.C.Syntax.Contants
  --deriveM (undefined :: CIntFlag)
  --deriveM (undefined :: CIntRepr)
  --deriveM (undefined :: Flags CIntFlag)
  --deriveM (undefined :: CInteger)
  --deriveM (undefined :: CChar)
  --deriveM (undefined :: CFloat)
  --deriveM (undefined :: CString)
