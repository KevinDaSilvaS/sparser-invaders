module Unit.Src.Transpiler.LexicalAnalyserSpec where

import Test.Hspec
import Transpiler.LexicalAnalyser

getTokenSpec :: IO ()
getTokenSpec = hspec $ do
    describe "Transpiler.LexicalAnalyser getToken" $ do
        it "should call getToken and get an Empty token" $ do
            let (token, remain, line, col) = getToken ([], 1, 0)
            snd token `shouldBe` ("" :: [Char])
            show (fst token) `shouldBe` ("Empty" :: [Char])
            remain `shouldBe` ([] :: [Char])
            line `shouldBe` (1 :: Int)
            col `shouldBe`  (0 :: Int)

        it "should call getToken and get an OpenObjToken token" $ do
            let (token, remain, line, col) = getToken ("{ \"key\": 12 }", 1, 0)
            snd token `shouldBe` ("{" :: [Char])
            show (fst token) `shouldBe` ("OpenObjToken" :: [Char])
            remain `shouldBe` (" \"key\": 12 }" :: [Char])
            line `shouldBe` (1 :: Int)
            col `shouldBe`  (0 :: Int)

        it "should call getToken and get an CloseObjToken token" $ do
            let (token, remain, line, col) = getToken ("}", 1, 0)
            snd token `shouldBe` ("}" :: [Char])
            show (fst token) `shouldBe` ("CloseObjToken" :: [Char])
            remain `shouldBe` ([] :: [Char])
            line `shouldBe` (1 :: Int)
            col `shouldBe`  (0:: Int)

        it "should call getToken and get an OpenArrayToken token" $ do
            let (token, remain, line, col) = getToken ("[", 1, 0)
            snd token `shouldBe` ("[" :: [Char])
            show (fst token) `shouldBe` ("OpenArrayToken" :: [Char])
            remain `shouldBe` ("" :: [Char])
            line `shouldBe` (1 :: Int)
            col `shouldBe`  (0 :: Int)
    
        it "should call getToken and get an CloseArrayToken token" $ do
            let (token, remain, line, col) = getToken ("]", 1, 0)
            snd token `shouldBe` ("]" :: [Char])
            show (fst token) `shouldBe` ("CloseArrayToken" :: [Char])
            remain `shouldBe` ([] :: [Char])
            line `shouldBe` (1 :: Int)
            col `shouldBe`  (0 :: Int)

        it "should call getToken and get an IdentifierKeyToken token" $ do
            let (token, remain, line, col) = getToken ("\"key\":", 1, 0)
            snd token `shouldBe` ("\"key\":" :: [Char])
            show (fst token) `shouldBe` ("IdentifierKeyToken" :: [Char])
            remain `shouldBe` ([] :: [Char])
            line `shouldBe` (1 :: Int)
            col `shouldBe`  (5 :: Int)

        it "should call getToken and get an StringToken token" $ do
            let (token, remain, line, col) = getToken ("\"value\"", 1, 0)
            snd token `shouldBe` ("\"value\"" :: [Char])
            show (fst token) `shouldBe` ("StringToken" :: [Char])
            remain `shouldBe` ([] :: [Char])
            line `shouldBe` (1 :: Int)
            col `shouldBe`  (7 :: Int)

        it "should call getToken and get an NumberToken token" $ do
            let (token, remain, line, col) = getToken ("12 }", 1, 0)
            snd token `shouldBe` ("12" :: [Char])
            show (fst token) `shouldBe` ("NumberToken" :: [Char])
            remain `shouldBe` (" }" :: [Char])
            line `shouldBe` (1 :: Int)
            col `shouldBe`  (2 :: Int)

        it "should call getToken and get an NullToken token" $ do
            let (token, remain, line, col) = getToken ("null", 1, 0)
            snd token `shouldBe` ("null" :: [Char])
            show (fst token) `shouldBe` ("NullToken" :: [Char])
            remain `shouldBe` ([] :: [Char])
            line `shouldBe` (1 :: Int)
            col `shouldBe`  (3 :: Int)

        it "should call getToken and get an BooleanToken token" $ do
            let (token, remain, line, col) = getToken ("true", 1, 0)
            snd token `shouldBe` ("true" :: [Char])
            show (fst token) `shouldBe` ("BooleanToken" :: [Char])
            remain `shouldBe` ([] :: [Char])
            line `shouldBe` (1 :: Int)
            col `shouldBe`  (3 :: Int)

        it "should call getToken and get an SeparatorToken token" $ do
            let (token, remain, line, col) = getToken (", 12]", 1, 0)
            snd token `shouldBe` ("," :: [Char])
            show (fst token) `shouldBe` ("SeparatorToken" :: [Char])
            remain `shouldBe` (" 12]" :: [Char])
            line `shouldBe` (1 :: Int)
            col `shouldBe`  (0 :: Int)