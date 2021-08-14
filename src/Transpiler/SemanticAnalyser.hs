module Transpiler.SemanticAnalyser where

import Transpiler.TokensJson
import Transpiler.IRBuilder

startSemanticAnalysis tokens = (thd, fth)
    where
        (_, _, thd, fth) = lookKeys tokens [] [] []
    
lookKeys [] _ warnings ir = ([], [], warnings, ir)
lookKeys ((OpenObjToken, x):xs) scope warnings ir = 
    lookKeys first scope thd fth
    where 
        (first, _, thd, fth) = 
            lookKeys xs [] warnings (ir ++ [irBuilder (OpenObjToken, x)])
lookKeys ((CloseObjToken, x):xs) scope warnings ir = 
    (xs, scope, warnings, ir ++ [irBuilder (CloseObjToken, x)])
lookKeys ((IdentifierKeyToken, x):xs) scope warnings ir 
    | x `elem` scope = 
        lookKeys xs scope 
        (warnings ++ [x ++ " is already declared."]) 
        (ir ++ [irBuilder (IdentifierKeyToken, x)])
    | otherwise = 
        lookKeys xs (scope++[x]) warnings 
        (ir ++ [irBuilder (IdentifierKeyToken, x)])
lookKeys ((_, ""):xs) scope warnings ir = 
    lookKeys xs scope warnings ir
lookKeys (x:xs) scope warnings ir = 
    lookKeys xs scope warnings (ir ++ [irBuilder x])
