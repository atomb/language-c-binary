module Language.C.Syntax.ManualBinaryInstances where

import Control.Monad
import Data.Binary
import Language.C.Data.Ident
import Language.C.Data.Name
import Language.C.Data.Node
import Language.C.Data.Position
import Language.C.Syntax.Constants

instance Binary Ident where
  put (Ident name uid inf) = put name >> put uid >> put inf
  get = liftM3 Ident get get get

instance Binary NodeInfo where
  put (OnlyPos p l) = put p >> put l >> put (Nothing :: Maybe Name)
  put (NodeInfo p l n) = put p >> put l >> put (Just n)
  get = liftM3 mkNode get get get
          where mkNode p l mb = maybe (OnlyPos p l) (NodeInfo p l) mb

instance Binary Position where
  put p
    | isNoPos p       = putWord8 0
    | isBuiltinPos p  = putWord8 1
    | isInternalPos p = putWord8 2
    | isSourcePos p   = do putWord8 3
                           put (posOffset p)
                           put (posFile p)
                           put (posRow p)
                           put (posColumn p)
    | otherwise       = fail "impossible"

  get = do _tag <- getWord8
           case _tag of
             0 -> return nopos
             1 -> return builtinPos
             2 -> return internalPos
             3 -> liftM4 position get get get get
             _ -> fail "no parse"

instance Binary Name where
  put p = put (nameId p)
  get   = liftM Name get

instance Binary CInteger where
  put (CInteger i r fs) = put i >> put r >> put fs
  get = liftM3 CInteger get get get

instance Binary CIntFlag where
  put FlagUnsigned = putWord8 0
  put FlagLong = putWord8 1
  put FlagLongLong = putWord8 2
  put FlagImag = putWord8 3
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> return FlagUnsigned
      1 -> return FlagLong
      2 -> return FlagLongLong
      3 -> return FlagImag
      _ -> fail "no parse"

instance Binary CIntRepr where
  put DecRepr = putWord8 0
  put HexRepr = putWord8 1
  put OctalRepr = putWord8 2
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> return DecRepr
      1 -> return HexRepr
      2 -> return OctalRepr
      _ -> fail "no parse"

instance (Binary a) => Binary (Flags a) where
  put (Flags a) = put a
  get = get >>= \a -> return (Flags a)

instance Binary CChar where
  put (CChar c b) = putWord8 0 >> put c >> put b
  put (CChars cs b) = putWord8 1 >> put cs >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> liftM2 CChar get get
      1 -> liftM2 CChars get get
      _ -> fail "no parse"

instance Binary CFloat where
  put (CFloat s) = put s
  get = liftM CFloat get

instance Binary CString where
  put (CString s b) = put s >> put b
  get = liftM2 CString get get
