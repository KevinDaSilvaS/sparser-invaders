module Transpiler.SintaticAnalyser where

import Transpiler.TokensJson ( TokenJson )
import Transpiler.LexicalAnalyser ( getToken )
import Transpiler.Helpers ( isValue, isKey )

startAnalysis :: [Char] -> (Bool, [(TokenJson, [Char])])
startAnalysis program = _A (program, 1, 0) 0 0 []

_A ([], _, _) 0 0 tokensList = (True, tokensList)
_A (program, line, col) stackObjs stackArrs tokensList
    | currToken == "{" && null list = error 
        ("[Sintatic Analysis] Unexpected end of file in line: " 
        ++ show nLine ++ " col: " ++ show nCol)
    | currToken == "{" && isKey nextToken      = 
        _K (list, nLine, nCol) (stackObjs+1) stackArrs nTokensList
    | currToken == "{" && snd nextToken == "}" = 
        _A (list, nLine, nCol) (stackObjs+1) stackArrs nTokensList
    | currToken == "[" && null list = error 
        ("[Sintatic Analysis] Unexpected end of file in line: " 
        ++ show nLine ++ " col: " ++ show nCol)
    | currToken == "[" && isValue nextToken    = 
        _T (list, nLine, nCol) stackObjs (stackArrs+1) nTokensList
    | currToken == "[" = 
        _A (list, nLine, nCol) stackObjs (stackArrs+1) nTokensList
    | currToken == "}" && snd nextToken == "," = 
        _E (list, nLine, nCol) (stackObjs-1) stackArrs nTokensList
    | currToken == "}" && (snd nextToken == "]" 
    || snd nextToken == "}" || snd nextToken == "") = 
        _A (list, nLine, nCol) (stackObjs-1) stackArrs nTokensList
    | currToken == "]" && snd nextToken == "," = 
        _E (list, nLine, nCol) stackObjs (stackArrs-1) nTokensList
    | currToken == "]" && (snd nextToken == "]" 
        || snd nextToken == "}" || snd nextToken == "") = 
        _A (list, nLine, nCol) stackObjs (stackArrs-1) nTokensList
    | otherwise = error 
        ("[Sintatic Analysis] Unexpected token " ++ show nextToken
        ++ " in line: " ++ show nNLine ++ " col: " ++ show nNCol)
    where
        currToken = snd token
        (token, list, nLine, nCol) = getToken (program, line, col)
        (nextToken, nlist, nNLine, nNCol) = getToken (list, nLine, nCol)
        nTokensList = tokensList ++ [token]

_K ([], _, _) stackObjs stackArrs tokensList = error ("err: _K")
_K (program, line, col) stackObjs stackArrs tokensList
    | null list = error 
        ("[Sintatic Analysis] Not expecting end of file in line: " 
        ++ show nLine ++ " col: " ++ show nCol)
    | isKey token = _T (list, nLine, nCol) stackObjs stackArrs nTokensList
    | isValue token = _T (program, line, col) stackObjs stackArrs tokensList
    | otherwise = error 
        ("[Sintatic Analysis] Unexpected token " ++ show token
        ++ " in line: " ++ show nLine ++ " col: " ++ show nCol)
    where
        (token, list, nLine, nCol) = getToken (program, line, col)
        nTokensList = tokensList ++ [token]

_T ([], _, _) stackObjs stackArrs tokensList = error ("err: _T")
_T (program, line, col) stackObjs stackArrs tokensList
    | null list = error 
        ("[Sintatic Analysis] Not expecting end of file in line: " 
        ++ show nLine ++ " col: " ++ show nCol)
    | currToken == "{" || currToken == "[" = 
        _A (program, line, col) stackObjs stackArrs tokensList
    | isValue token = 
        _E (list, nLine, nCol) stackObjs stackArrs nTokensList
    | otherwise = error 
        ("[Sintatic Analysis] Expecting value but got: " ++ show token
        ++ " instead. in line: " ++ show nLine ++ " col: " ++ show nCol) 
    where
        currToken = snd token
        (token, list, nLine, nCol) = getToken (program, line, col)
        nTokensList = tokensList ++ [token]

_E ([], _, _) stackObjs stackArrs tokensList = error ("err: _E")
_E (program, line, col) stackObjs stackArrs tokensList
    | currToken == "," && (snd nextToken == "]" || snd nextToken == ",") = 
        error 
        ("[Sintatic Analysis] Expecting value or key but got: " ++ show nextToken
        ++ " instead. in line: " ++ show nNLine ++ " col: " ++ show nNCol) 
    | currToken == "," && isValue nextToken = 
        _T (list, nLine, nCol) stackObjs stackArrs nTokensList
    | currToken == "," = 
        _K (list, nLine, nCol) stackObjs stackArrs nTokensList
    | currToken == "}" || currToken == "]" = 
        _A (program, line, col) stackObjs stackArrs tokensList
    | otherwise = error 
        ("[Sintatic Analysis] Expecting ',', '}' or ']' but got: " ++ show token
        ++ " instead. in line: " ++ show nLine ++ " col: " ++ show nCol) 
    where
        currToken = snd token
        (token, list, nLine, nCol) = getToken (program, line, col)
        (nextToken, nlist, nNLine, nNCol) = getToken (list, nLine, nCol)
        nTokensList = tokensList ++ [token]