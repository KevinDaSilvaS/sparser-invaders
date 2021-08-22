module Unit.Src.Transpiler.HelpersSpec where

import Test.Hspec ( shouldBe, it, describe, hspec ) 

import Transpiler.TokensJson
    ( TokenJson(Empty, IdentifierKeyToken, NumberToken, BooleanToken,
                OpenArrayToken, OpenObjToken, NullToken, SeparatorToken,
                CloseArrayToken, CloseObjToken) )
import Transpiler.Helpers ( isKey, isValue ) 

helpersSpec :: IO ()
helpersSpec = do 
    helpersIsValueSpec 
    helpersIsKeySpec

helpersIsValueSpec :: IO ()
helpersIsValueSpec = hspec $ do
    describe "Transpiler.Helpers - True - IsValue" $ do
        it "should call isValue with (NumberToken, [Char]) and get True" $ do
            isValue (NumberToken, "12") 
                `shouldBe` (True :: Bool)

        it "should call isValue with (BooleanToken, [Char]) and get True" 
            $ do
                isValue (BooleanToken, "true") 
                `shouldBe` (True :: Bool)

        it "should call isValue with (OpenArrayToken, [Char]) and get True" 
            $ do
                isValue (OpenArrayToken, "[") 
                `shouldBe` (True :: Bool)

        it "should call isValue with (OpenObjToken, [Char]) and get True" 
            $ do
                isValue (OpenObjToken, "{") 
                `shouldBe` (True :: Bool)

        it "should call isValue with (NullToken, [Char]) and get True" 
            $ do
                isValue (NullToken, "null") 
                `shouldBe` (True :: Bool)

    describe "Transpiler.Helpers - False - IsValue" $ do
        it "should call isValue with (IdentifierKeyToken, [Char]) and get False" $ do
            isValue (IdentifierKeyToken, "\"key\":") 
                `shouldBe` (False :: Bool)

        it "should call isValue with (SeparatorToken, [Char]) and get False" $ do
            isValue (SeparatorToken, ",") 
                `shouldBe` (False :: Bool)

        it "should call isValue with (CloseArrayToken, [Char]) and get False" $ do
            isValue (CloseArrayToken, "]") 
                `shouldBe` (False :: Bool)

        it "should call isValue with (CloseObjToken, [Char]) and get False" $ do
            isValue (CloseObjToken, "}") 
                `shouldBe` (False :: Bool)

        it "should call isValue with (Empty, undefined) and get False" $ do
            isValue (Empty, undefined) 
                `shouldBe` (False :: Bool)

helpersIsKeySpec :: IO ()
helpersIsKeySpec = hspec $ do
        describe "Transpiler.Helpers - True - isKey" $ do
            it "should call isKey with (IdentifierKeyToken, [Char]) and get False" $ do
                isKey (IdentifierKeyToken, "\"key\":") 
                    `shouldBe` (True :: Bool)

        describe "Transpiler.Helpers - False - isKey" $ do
            it "should call isKey with (NumberToken, [Char]) and get False" $ do
                isKey (NumberToken, "12") 
                    `shouldBe` (False :: Bool)
            
            it "should call isKey with (BooleanToken, [Char]) and get False" 
                $ do
                    isKey (BooleanToken, "false") 
                        `shouldBe` (False :: Bool)
            
            it "should call isKey with (OpenArrayToken, [Char]) and get False" 
                $ do
                    isKey (OpenArrayToken, "[") 
                        `shouldBe` (False :: Bool)
            
            it "should call isKey with (OpenObjToken, [Char]) and get False" 
                $ do
                    isKey (OpenObjToken, "{") 
                        `shouldBe` (False :: Bool)
            
            it "should call isKey with (NullToken, [Char]) and get False" 
                $ do
                    isKey (NullToken, "null") 
                        `shouldBe` (False :: Bool)

            it "should call isKey with (SeparatorToken, [Char]) and get False" $ do
                isKey (SeparatorToken, ",") 
                    `shouldBe` (False :: Bool)
            
            it "should call isKey with (CloseArrayToken, [Char]) and get False" $ do
                isKey (CloseArrayToken, "]") 
                    `shouldBe` (False :: Bool)
            
            it "should call isKey with (CloseObjToken, [Char]) and get False" $ do
                isKey (CloseObjToken, "}") 
                    `shouldBe` (False :: Bool)
            
            it "should call isKey with (Empty, undefined) and get False" $ do
                isKey (Empty, undefined) 
                    `shouldBe` (False :: Bool)
        

