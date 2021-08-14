module Transpiler.Helpers where

import Transpiler.TokensJson

isValue (StringToken, _)    = True
isValue (NumberToken, _)    = True
isValue (BooleanToken, _)   = True
isValue (OpenArrayToken, _) = True
isValue (OpenObjToken, _)   = True
isValue (NullToken, _)      = True
isValue _ = False

isKey (IdentifierKeyToken, _) = True
isKey _ = False
