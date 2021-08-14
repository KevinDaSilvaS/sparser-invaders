module Transpiler.LexicalAnalyser where

import Transpiler.TokensJson

getToken ([], line, col) = ((Empty, ""), [], line, col)
getToken (program, line, col) = entrypoint program [] line col

entrypoint ('\n':xs) reading line col =
    entrypoint xs reading (line+1) 0
entrypoint (' ':xs) reading line col  =
    entrypoint xs reading line (col+1)
entrypoint ('{':xs) reading line col  = ((OpenObjToken, "{"), xs, line, col)
entrypoint ('}':xs) reading line col  = ((CloseObjToken, "}"), xs, line, col)
entrypoint ('[':xs) reading line col  = ((OpenArrayToken, "["), xs, line, col)
entrypoint (']':xs) reading line col  = ((CloseArrayToken, "]"), xs, line, col)
entrypoint (',':xs) reading line col  = ((SeparatorToken, ","), xs, line, col)
entrypoint ('t':xs) reading line col  =
    booleanAutomaton xs ['t'] line (col+1)
entrypoint ('f':xs) reading line col  =
    booleanAutomaton xs ['f'] line (col+1)
entrypoint ('n':xs) reading line col  =
    nullAutomaton xs ['n'] line (col+1)
entrypoint ('"':xs) reading line col  =
    stringAutomaton xs ['"'] line (col+1)
entrypoint xs _ line col = numberAutomaton xs [] line col

stringAutomaton ('\n':xs) reading line col = 
    error ("[Lexical Error] Line breaks are not allowed in keys and values "
    ++ show reading ++ ". line: " ++ show line
    ++ " col: " ++ show col)
stringAutomaton ('"':xs) reading line col =
    keyAutomaton xs (reading++['"']) line (col+1)
stringAutomaton (x:xs) reading line col =
    stringAutomaton xs (reading++[x]) line (col+1)
stringAutomaton [] reading line col =
    error ("[Lexical Error] String or Key token expected in line: "
    ++ show line ++ " col: " ++ show col ++ ".Received: " ++ reading)

keyAutomaton (':':xs) reading line col =
    ((IdentifierKeyToken, reading++[':']), xs, line, col)
keyAutomaton [] reading line col = 
    ((StringToken, reading), [], line, col)
keyAutomaton xs reading line col =
    ((StringToken, reading), xs, line, col)

numberAutomaton (x:xs) reading line col
    | x `elem` numerals = numberAutomaton xs (reading++[x]) line (col+1)
    | null reading = error 
    ("[Lexical Error] Value not expected: " 
    ++ show x ++ " in line"
    ++ show line ++ " col: " ++ show col)
    | x == '.' && not (null reading) =
        floatAutomaton xs (reading++[x]) line (col+1)
    | otherwise = ((NumberToken, reading), (x:xs), line, col)
    where
        numerals = ['0'..'9']

numberAutomaton _ _ line col = error
    ("[Lexical Error] Unknown error in line: "
    ++ show line ++ " col: " ++ show col)

floatAutomaton (x:xs) reading line col
    | x `elem` numerals = floatAutomaton xs (reading++[x]) line (col+1)
    | otherwise = ((NumberToken, reading), (x:xs), line, col)
    where
        numerals = ['0'..'9']
floatAutomaton _ _ line col = error
    ("[Lexical Error] Unknown error in line: "
    ++ show line ++ " col: " ++ show col)

booleanAutomaton ('r':xs) "t" line col    = 
    booleanAutomaton xs "tr" line (col+1)
booleanAutomaton ('u':xs) "tr" line col   = 
    booleanAutomaton xs "tru" line (col+1)
booleanAutomaton ('e':xs) "tru" line col  = 
    ((BooleanToken, "true"), xs, line, col)
booleanAutomaton ('a':xs) "f" line col    = 
    booleanAutomaton xs "fa" line (col+1)
booleanAutomaton ('l':xs) "fa" line col   = 
    booleanAutomaton xs "fal" line (col+1)
booleanAutomaton ('s':xs) "fal" line col  = 
    booleanAutomaton xs "fals" line (col+1)
booleanAutomaton ('e':xs) "fals" line col = 
    ((BooleanToken, "false"), xs, line, col)
booleanAutomaton _ reading line col       = 
    error ("[Lexical Error] Unexpected " 
    ++ show reading ++ " symbol (expecting true or false) in line: "
    ++ show line ++ " col: " ++ show col)

nullAutomaton ('u':xs) "n" line col = 
    nullAutomaton xs "nu" line (col+1)
nullAutomaton ('l':xs) "nu" line col = 
    nullAutomaton xs "nul" line (col+1)
nullAutomaton ('l':xs) "nul" line col = 
    ((NullToken, "null"), xs, line, col)
nullAutomaton _ reading line col       = 
    error ("[Lexical Error] Unexpected " 
    ++ show reading ++ " symbol (expecting null) in line: "
    ++ show line ++ " col: " ++ show col)