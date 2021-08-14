module Transpiler.IRBuilder where

import Transpiler.TokensJson ( TokenJson(IdentifierKeyToken) )

irBuilder (IdentifierKeyToken, x) = (init $ init $ drop 1 x) ++ [last x]
irBuilder (_, x) = x