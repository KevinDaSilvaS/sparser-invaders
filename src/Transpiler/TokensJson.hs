module Transpiler.TokensJson where

data TokenJson = 
            OpenObjToken       | 
            CloseObjToken      | 
            IdentifierKeyToken |
            StringToken        |
            NumberToken        |
            NullToken          |
            OpenArrayToken     |
            CloseArrayToken    |
            BooleanToken       |
            SeparatorToken     |
            Empty
            deriving(Show)