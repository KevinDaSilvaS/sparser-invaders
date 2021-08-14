module Transpiler.GenerateJSCode where

import System.IO

allowedStartKeyChars = ['A'..'z']

genJSFile :: [[Char]] -> [Char] -> IO ()
genJSFile ir path = do
    let lb = map addLB ir
    let contents = "module.exports = "
                    ++ concat lb
                    ++ ";"
    writeFile (path++".js") contents

addLB x
    | last x == ':' && notElem (head x) allowedStartKeyChars =
        "\'" ++ init x ++ "\'" ++ [last x]
    | x == "{" || x == "," =  x++"\n"
    | x == "}" = "\n"++x
    | otherwise = x
