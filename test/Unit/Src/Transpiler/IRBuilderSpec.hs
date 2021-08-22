module Unit.Src.Transpiler.IRBuilderSpec where

import Test.Hspec 

import Transpiler.TokensJson
    ( TokenJson(SeparatorToken, IdentifierKeyToken) ) 
import Transpiler.IRBuilder ( irBuilder )

iRBuilderSpec :: IO ()
iRBuilderSpec = hspec $ do
    describe "Transpiler.IRBuilder - Success" $ do
        it "should call irBuilder with (IdentifierKeyToken, [Char])" $ do
            irBuilder (IdentifierKeyToken, "\"key\":") 
                `shouldBe` ("key:" :: [Char])

        it "should call irBuilder with any other token type" $ do
            irBuilder (SeparatorToken, ",") `shouldBe` ("," :: [Char])

        it "should call irBuilder with any other token type and a generic list with any oher type" 
            $ do
            irBuilder (SeparatorToken, [1]) `shouldBe` ([1] :: [Int])

        it "should call irBuilder with IdentifierKeyToken and a generic list with any oher type that have a length size of at least 4" 
            $ do
            irBuilder (IdentifierKeyToken, [1,2,3,4]) `shouldBe` ([2,4] :: [Int])
