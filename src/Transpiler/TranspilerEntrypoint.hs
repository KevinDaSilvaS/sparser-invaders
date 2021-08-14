module Transpiler.TranspilerEntrypoint where 

import Control.Exception ( try, SomeException ) 
import Transpiler.LexicalAnalyser ( getToken )
import Transpiler.SintaticAnalyser ( startAnalysis )
import Transpiler.SemanticAnalyser ( startSemanticAnalysis )
import Transpiler.GenerateJSCode ( genJSFile )
import Transpiler.GenerateYmlCode ( genYmlFile ) 
import Transpiler.TokensJson ( TokenJson )
import OutputFormater.FormatMessage ( _error, _warning, _success )

startParsing :: [FilePath] -> IO ()
startParsing [path, format] = do
    let pathExtension = reverse $ take 5 (reverse path)
    if pathExtension /= ".json" then do
        putStrLn $ 
            _error "Expected a valid path containing a file with .json extension"
    else do
        contents <- try (readFile path) :: IO (Either SomeException [Char])
        case contents of
            Right file ->
                if null file then
                    putStrLn $ _error "[Transpiler] File is empty"
                else do
                    parsingResult <- try (parsingFlowSteps file path format)
                        :: IO (Either SomeException ())
                    case parsingResult of
                        Right file -> do return ()
                        Left e -> do 
                            putStrLn (_error $ show e)
            Left e -> do
                putStrLn $ _error "[File not found] Error file not found"
startParsing _ = do
    putStrLn $ _error
        "[CLI - Transpiler] Error expecting filepath and format to be converted"

parsingFlowSteps :: [Char] -> [Char] -> [Char] -> IO ()
parsingFlowSteps program path format = do
    let tokens = startAnalysis program
    let (warnings, ir) = startSemanticAnalysis (snd tokens)
    mapM_ (putStrLn . _warning) warnings
    let pathWithoutExtension = reverse $ drop 5 (reverse path)
    startTranslation ir pathWithoutExtension format
    putStrLn $ _success "Json successfully parsed"
    return ()

startTranslation :: [[Char]] -> [Char] -> [Char] -> IO ()
startTranslation ir path "*" = do
    genJSFile ir path
    genYmlFile ir path
startTranslation ir path "js" = do
    genJSFile ir path
startTranslation ir path "yml" = do
    genYmlFile ir path
startTranslation _ _ _ = error
    "[Transpiler - Translation Format] Error choosed format is not supported"


