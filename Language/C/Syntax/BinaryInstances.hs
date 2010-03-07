module Language.C.Syntax.BinaryInstances where

import Data.Binary
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Syntax.Ops
import Language.C.Syntax.ManualBinaryInstances

instance (Binary a) => Binary (Language.C.Syntax.AST.CTranslationUnit a) where
  put (CTranslUnit a b) = put a >> put b
  get = get >>= \a -> get >>= \b -> return (CTranslUnit a b)

instance (Binary a) => Binary (Language.C.Syntax.AST.CExternalDeclaration a) where
  put (CDeclExt a) = putWord8 0 >> put a
  put (CFDefExt a) = putWord8 1 >> put a
  put (CAsmExt a b) = putWord8 2 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (CDeclExt a)
      1 -> get >>= \a -> return (CFDefExt a)
      2 -> get >>= \a -> get >>= \b -> return (CAsmExt a b)
      _ -> fail "no parse"

instance (Binary a) => Binary (Language.C.Syntax.AST.CFunctionDef a) where
  put (CFunDef a b c d e) = put a >> put b >> put c >> put d >> put e
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> return (CFunDef a b c d e)

instance (Binary a) => Binary (Language.C.Syntax.AST.CDeclaration a) where
  put (CDecl a b c) = put a >> put b >> put c
  get = get >>= \a -> get >>= \b -> get >>= \c -> return (CDecl a b c)

instance (Binary a) => Binary (Language.C.Syntax.AST.CStructureUnion a) where
  put (CStruct a b c d e) = put a >> put b >> put c >> put d >> put e
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> return (CStruct a b c d e)

instance Binary Language.C.Syntax.AST.CStructTag where
  put CStructTag = putWord8 0
  put CUnionTag = putWord8 1
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> return CStructTag
      1 -> return CUnionTag
      _ -> fail "no parse"

instance (Binary a) => Binary (Language.C.Syntax.AST.CEnumeration a) where
  put (CEnum a b c d) = put a >> put b >> put c >> put d
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (CEnum a b c d)

instance (Binary a) => Binary (Language.C.Syntax.AST.CDeclarationSpecifier a) where
  put (CStorageSpec a) = putWord8 0 >> put a
  put (CTypeSpec a) = putWord8 1 >> put a
  put (CTypeQual a) = putWord8 2 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (CStorageSpec a)
      1 -> get >>= \a -> return (CTypeSpec a)
      2 -> get >>= \a -> return (CTypeQual a)
      _ -> fail "no parse"

instance (Binary a) => Binary (Language.C.Syntax.AST.CStorageSpecifier a) where
  put (CAuto a) = putWord8 0 >> put a
  put (CRegister a) = putWord8 1 >> put a
  put (CStatic a) = putWord8 2 >> put a
  put (CExtern a) = putWord8 3 >> put a
  put (CTypedef a) = putWord8 4 >> put a
  put (CThread a) = putWord8 5 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (CAuto a)
      1 -> get >>= \a -> return (CRegister a)
      2 -> get >>= \a -> return (CStatic a)
      3 -> get >>= \a -> return (CExtern a)
      4 -> get >>= \a -> return (CTypedef a)
      5 -> get >>= \a -> return (CThread a)
      _ -> fail "no parse"

instance (Binary a) => Binary (Language.C.Syntax.AST.CTypeSpecifier a) where
  put (CVoidType a) = putWord8 0 >> put a
  put (CCharType a) = putWord8 1 >> put a
  put (CShortType a) = putWord8 2 >> put a
  put (CIntType a) = putWord8 3 >> put a
  put (CLongType a) = putWord8 4 >> put a
  put (CFloatType a) = putWord8 5 >> put a
  put (CDoubleType a) = putWord8 6 >> put a
  put (CSignedType a) = putWord8 7 >> put a
  put (CUnsigType a) = putWord8 8 >> put a
  put (CBoolType a) = putWord8 9 >> put a
  put (CComplexType a) = putWord8 10 >> put a
  put (CSUType a b) = putWord8 11 >> put a >> put b
  put (CEnumType a b) = putWord8 12 >> put a >> put b
  put (CTypeDef a b) = putWord8 13 >> put a >> put b
  put (CTypeOfExpr a b) = putWord8 14 >> put a >> put b
  put (CTypeOfType a b) = putWord8 15 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (CVoidType a)
      1 -> get >>= \a -> return (CCharType a)
      2 -> get >>= \a -> return (CShortType a)
      3 -> get >>= \a -> return (CIntType a)
      4 -> get >>= \a -> return (CLongType a)
      5 -> get >>= \a -> return (CFloatType a)
      6 -> get >>= \a -> return (CDoubleType a)
      7 -> get >>= \a -> return (CSignedType a)
      8 -> get >>= \a -> return (CUnsigType a)
      9 -> get >>= \a -> return (CBoolType a)
      10 -> get >>= \a -> return (CComplexType a)
      11 -> get >>= \a -> get >>= \b -> return (CSUType a b)
      12 -> get >>= \a -> get >>= \b -> return (CEnumType a b)
      13 -> get >>= \a -> get >>= \b -> return (CTypeDef a b)
      14 -> get >>= \a -> get >>= \b -> return (CTypeOfExpr a b)
      15 -> get >>= \a -> get >>= \b -> return (CTypeOfType a b)
      _ -> fail "no parse"

instance (Binary a) => Binary (Language.C.Syntax.AST.CTypeQualifier a) where
  put (CConstQual a) = putWord8 0 >> put a
  put (CVolatQual a) = putWord8 1 >> put a
  put (CRestrQual a) = putWord8 2 >> put a
  put (CInlineQual a) = putWord8 3 >> put a
  put (CAttrQual a) = putWord8 4 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (CConstQual a)
      1 -> get >>= \a -> return (CVolatQual a)
      2 -> get >>= \a -> return (CRestrQual a)
      3 -> get >>= \a -> return (CInlineQual a)
      4 -> get >>= \a -> return (CAttrQual a)
      _ -> fail "no parse"

instance (Binary a) => Binary (Language.C.Syntax.AST.CAttribute a) where
  put (CAttr a b c) = put a >> put b >> put c
  get = get >>= \a -> get >>= \b -> get >>= \c -> return (CAttr a b c)

instance (Binary a) => Binary (Language.C.Syntax.AST.CDeclarator a) where
  put (CDeclr a b c d e) = put a >> put b >> put c >> put d >> put e
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> return (CDeclr a b c d e)

instance (Binary a) => Binary (Language.C.Syntax.AST.CDerivedDeclarator a) where
  put (CPtrDeclr a b) = putWord8 0 >> put a >> put b
  put (CArrDeclr a b c) = putWord8 1 >> put a >> put b >> put c
  put (CFunDeclr a b c) = putWord8 2 >> put a >> put b >> put c
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (CPtrDeclr a b)
      1 -> get >>= \a -> get >>= \b -> get >>= \c -> return (CArrDeclr a b c)
      2 -> get >>= \a -> get >>= \b -> get >>= \c -> return (CFunDeclr a b c)
      _ -> fail "no parse"

instance (Binary a) => Binary (Language.C.Syntax.AST.CArraySize a) where
  put (CNoArrSize a) = putWord8 0 >> put a
  put (CArrSize a b) = putWord8 1 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (CNoArrSize a)
      1 -> get >>= \a -> get >>= \b -> return (CArrSize a b)
      _ -> fail "no parse"

instance (Binary a) => Binary (Language.C.Syntax.AST.CInitializer a) where
  put (CInitExpr a b) = putWord8 0 >> put a >> put b
  put (CInitList a b) = putWord8 1 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (CInitExpr a b)
      1 -> get >>= \a -> get >>= \b -> return (CInitList a b)
      _ -> fail "no parse"

instance (Binary a) => Binary (Language.C.Syntax.AST.CPartDesignator a) where
  put (CArrDesig a b) = putWord8 0 >> put a >> put b
  put (CMemberDesig a b) = putWord8 1 >> put a >> put b
  put (CRangeDesig a b c) = putWord8 2 >> put a >> put b >> put c
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (CArrDesig a b)
      1 -> get >>= \a -> get >>= \b -> return (CMemberDesig a b)
      2 -> get >>= \a -> get >>= \b -> get >>= \c -> return (CRangeDesig a b c)
      _ -> fail "no parse"

instance (Binary a) => Binary (Language.C.Syntax.AST.CStatement a) where
  put (CLabel a b c d) = putWord8 0 >> put a >> put b >> put c >> put d
  put (CCase a b c) = putWord8 1 >> put a >> put b >> put c
  put (CCases a b c d) = putWord8 2 >> put a >> put b >> put c >> put d
  put (CDefault a b) = putWord8 3 >> put a >> put b
  put (CExpr a b) = putWord8 4 >> put a >> put b
  put (CCompound a b c) = putWord8 5 >> put a >> put b >> put c
  put (CIf a b c d) = putWord8 6 >> put a >> put b >> put c >> put d
  put (CSwitch a b c) = putWord8 7 >> put a >> put b >> put c
  put (CWhile a b c d) = putWord8 8 >> put a >> put b >> put c >> put d
  put (CFor a b c d e) = putWord8 9 >> put a >> put b >> put c >> put d >> put e
  put (CGoto a b) = putWord8 10 >> put a >> put b
  put (CGotoPtr a b) = putWord8 11 >> put a >> put b
  put (CCont a) = putWord8 12 >> put a
  put (CBreak a) = putWord8 13 >> put a
  put (CReturn a b) = putWord8 14 >> put a >> put b
  put (CAsm a b) = putWord8 15 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (CLabel a b c d)
      1 -> get >>= \a -> get >>= \b -> get >>= \c -> return (CCase a b c)
      2 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (CCases a b c d)
      3 -> get >>= \a -> get >>= \b -> return (CDefault a b)
      4 -> get >>= \a -> get >>= \b -> return (CExpr a b)
      5 -> get >>= \a -> get >>= \b -> get >>= \c -> return (CCompound a b c)
      6 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (CIf a b c d)
      7 -> get >>= \a -> get >>= \b -> get >>= \c -> return (CSwitch a b c)
      8 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (CWhile a b c d)
      9 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> return (CFor a b c d e)
      10 -> get >>= \a -> get >>= \b -> return (CGoto a b)
      11 -> get >>= \a -> get >>= \b -> return (CGotoPtr a b)
      12 -> get >>= \a -> return (CCont a)
      13 -> get >>= \a -> return (CBreak a)
      14 -> get >>= \a -> get >>= \b -> return (CReturn a b)
      15 -> get >>= \a -> get >>= \b -> return (CAsm a b)
      _ -> fail "no parse"

instance (Binary a) => Binary (Language.C.Syntax.AST.CCompoundBlockItem a) where
  put (CBlockStmt a) = putWord8 0 >> put a
  put (CBlockDecl a) = putWord8 1 >> put a
  put (CNestedFunDef a) = putWord8 2 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (CBlockStmt a)
      1 -> get >>= \a -> return (CBlockDecl a)
      2 -> get >>= \a -> return (CNestedFunDef a)
      _ -> fail "no parse"

instance (Binary a) => Binary (Language.C.Syntax.AST.CAssemblyStatement a) where
  put (CAsmStmt a b c d e f) = put a >> put b >> put c >> put d >> put e >> put f
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> get >>= \f -> return (CAsmStmt a b c d e f)

instance (Binary a) => Binary (Language.C.Syntax.AST.CAssemblyOperand a) where
  put (CAsmOperand a b c d) = put a >> put b >> put c >> put d
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (CAsmOperand a b c d)

instance (Binary a) => Binary (Language.C.Syntax.AST.CExpression a) where
  put (CComma a b) = putWord8 0 >> put a >> put b
  put (CAssign a b c d) = putWord8 1 >> put a >> put b >> put c >> put d
  put (CCond a b c d) = putWord8 2 >> put a >> put b >> put c >> put d
  put (CBinary a b c d) = putWord8 3 >> put a >> put b >> put c >> put d
  put (CCast a b c) = putWord8 4 >> put a >> put b >> put c
  put (CUnary a b c) = putWord8 5 >> put a >> put b >> put c
  put (CSizeofExpr a b) = putWord8 6 >> put a >> put b
  put (CSizeofType a b) = putWord8 7 >> put a >> put b
  put (CAlignofExpr a b) = putWord8 8 >> put a >> put b
  put (CAlignofType a b) = putWord8 9 >> put a >> put b
  put (CComplexReal a b) = putWord8 10 >> put a >> put b
  put (CComplexImag a b) = putWord8 11 >> put a >> put b
  put (CIndex a b c) = putWord8 12 >> put a >> put b >> put c
  put (CCall a b c) = putWord8 13 >> put a >> put b >> put c
  put (CMember a b c d) = putWord8 14 >> put a >> put b >> put c >> put d
  put (CVar a b) = putWord8 15 >> put a >> put b
  put (CConst a) = putWord8 16 >> put a
  put (CCompoundLit a b c) = putWord8 17 >> put a >> put b >> put c
  put (CStatExpr a b) = putWord8 18 >> put a >> put b
  put (CLabAddrExpr a b) = putWord8 19 >> put a >> put b
  put (CBuiltinExpr a) = putWord8 20 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (CComma a b)
      1 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (CAssign a b c d)
      2 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (CCond a b c d)
      3 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (CBinary a b c d)
      4 -> get >>= \a -> get >>= \b -> get >>= \c -> return (CCast a b c)
      5 -> get >>= \a -> get >>= \b -> get >>= \c -> return (CUnary a b c)
      6 -> get >>= \a -> get >>= \b -> return (CSizeofExpr a b)
      7 -> get >>= \a -> get >>= \b -> return (CSizeofType a b)
      8 -> get >>= \a -> get >>= \b -> return (CAlignofExpr a b)
      9 -> get >>= \a -> get >>= \b -> return (CAlignofType a b)
      10 -> get >>= \a -> get >>= \b -> return (CComplexReal a b)
      11 -> get >>= \a -> get >>= \b -> return (CComplexImag a b)
      12 -> get >>= \a -> get >>= \b -> get >>= \c -> return (CIndex a b c)
      13 -> get >>= \a -> get >>= \b -> get >>= \c -> return (CCall a b c)
      14 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (CMember a b c d)
      15 -> get >>= \a -> get >>= \b -> return (CVar a b)
      16 -> get >>= \a -> return (CConst a)
      17 -> get >>= \a -> get >>= \b -> get >>= \c -> return (CCompoundLit a b c)
      18 -> get >>= \a -> get >>= \b -> return (CStatExpr a b)
      19 -> get >>= \a -> get >>= \b -> return (CLabAddrExpr a b)
      20 -> get >>= \a -> return (CBuiltinExpr a)
      _ -> fail "no parse"

instance Binary Language.C.Syntax.Ops.CAssignOp where
  put CAssignOp = putWord8 0
  put CMulAssOp = putWord8 1
  put CDivAssOp = putWord8 2
  put CRmdAssOp = putWord8 3
  put CAddAssOp = putWord8 4
  put CSubAssOp = putWord8 5
  put CShlAssOp = putWord8 6
  put CShrAssOp = putWord8 7
  put CAndAssOp = putWord8 8
  put CXorAssOp = putWord8 9
  put COrAssOp = putWord8 10
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> return CAssignOp
      1 -> return CMulAssOp
      2 -> return CDivAssOp
      3 -> return CRmdAssOp
      4 -> return CAddAssOp
      5 -> return CSubAssOp
      6 -> return CShlAssOp
      7 -> return CShrAssOp
      8 -> return CAndAssOp
      9 -> return CXorAssOp
      10 -> return COrAssOp
      _ -> fail "no parse"

instance Binary Language.C.Syntax.Ops.CBinaryOp where
  put CMulOp = putWord8 0
  put CDivOp = putWord8 1
  put CRmdOp = putWord8 2
  put CAddOp = putWord8 3
  put CSubOp = putWord8 4
  put CShlOp = putWord8 5
  put CShrOp = putWord8 6
  put CLeOp = putWord8 7
  put CGrOp = putWord8 8
  put CLeqOp = putWord8 9
  put CGeqOp = putWord8 10
  put CEqOp = putWord8 11
  put CNeqOp = putWord8 12
  put CAndOp = putWord8 13
  put CXorOp = putWord8 14
  put COrOp = putWord8 15
  put CLndOp = putWord8 16
  put CLorOp = putWord8 17
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> return CMulOp
      1 -> return CDivOp
      2 -> return CRmdOp
      3 -> return CAddOp
      4 -> return CSubOp
      5 -> return CShlOp
      6 -> return CShrOp
      7 -> return CLeOp
      8 -> return CGrOp
      9 -> return CLeqOp
      10 -> return CGeqOp
      11 -> return CEqOp
      12 -> return CNeqOp
      13 -> return CAndOp
      14 -> return CXorOp
      15 -> return COrOp
      16 -> return CLndOp
      17 -> return CLorOp
      _ -> fail "no parse"

instance Binary Language.C.Syntax.Ops.CUnaryOp where
  put CPreIncOp = putWord8 0
  put CPreDecOp = putWord8 1
  put CPostIncOp = putWord8 2
  put CPostDecOp = putWord8 3
  put CAdrOp = putWord8 4
  put CIndOp = putWord8 5
  put CPlusOp = putWord8 6
  put CMinOp = putWord8 7
  put CCompOp = putWord8 8
  put CNegOp = putWord8 9
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> return CPreIncOp
      1 -> return CPreDecOp
      2 -> return CPostIncOp
      3 -> return CPostDecOp
      4 -> return CAdrOp
      5 -> return CIndOp
      6 -> return CPlusOp
      7 -> return CMinOp
      8 -> return CCompOp
      9 -> return CNegOp
      _ -> fail "no parse"

instance (Binary a) => Binary (Language.C.Syntax.AST.CBuiltinThing a) where
  put (CBuiltinVaArg a b c) = putWord8 0 >> put a >> put b >> put c
  put (CBuiltinOffsetOf a b c) = putWord8 1 >> put a >> put b >> put c
  put (CBuiltinTypesCompatible a b c) = putWord8 2 >> put a >> put b >> put c
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> get >>= \c -> return (CBuiltinVaArg a b c)
      1 -> get >>= \a -> get >>= \b -> get >>= \c -> return (CBuiltinOffsetOf a b c)
      2 -> get >>= \a -> get >>= \b -> get >>= \c -> return (CBuiltinTypesCompatible a b c)
      _ -> fail "no parse"

instance (Binary a) => Binary (Language.C.Syntax.AST.CConstant a) where
  put (CIntConst a b) = putWord8 0 >> put a >> put b
  put (CCharConst a b) = putWord8 1 >> put a >> put b
  put (CFloatConst a b) = putWord8 2 >> put a >> put b
  put (CStrConst a b) = putWord8 3 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (CIntConst a b)
      1 -> get >>= \a -> get >>= \b -> return (CCharConst a b)
      2 -> get >>= \a -> get >>= \b -> return (CFloatConst a b)
      3 -> get >>= \a -> get >>= \b -> return (CStrConst a b)
      _ -> fail "no parse"

instance (Binary a) => Binary (Language.C.Syntax.AST.CStringLiteral a) where
  put (CStrLit a b) = put a >> put b
  get = get >>= \a -> get >>= \b -> return (CStrLit a b)

