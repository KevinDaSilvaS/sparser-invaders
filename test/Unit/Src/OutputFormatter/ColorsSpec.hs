module Unit.Src.OutputFormatter.ColorsSpec where

import Test.Hspec ( hspec, describe, it, shouldBe )
import OutputFormatter.Colors
    ( textInfo,
      backgroundInfo,
      reset,
      backgroundError,
      backgroundSuccess,
      backgroundWarning,
      textBlack,
      textError,
      textSuccess,
      textWarning )
    
colorsSpec :: IO ()
colorsSpec = hspec $ do
    describe "OutputFormatter.Colors - Success" $ do
        it "should call backgroundError" $ do
            backgroundError `shouldBe` ("\x1b[41m" :: [Char])

        it "should call backgroundWarning" $ do
            backgroundWarning `shouldBe` (("\x1b[43m " ++ textBlack) :: [Char])
        
        it "should call backgroundInfo" $ do
            backgroundInfo `shouldBe` ("\x1b[44m \x1b[37;1m" :: [Char])

        it "should call backgroundSuccess" $ do
            backgroundSuccess `shouldBe` ("\x1b[42;1m " :: [Char])

        it "should call textError" $ do
            textError `shouldBe` ("\x1b[31m" :: [Char])

        it "should call textWarning" $ do
            textWarning `shouldBe` ("\x1b[33m" :: [Char])

        it "should call textInfo" $ do
            textInfo `shouldBe` ("\x1b[36m" :: [Char])

        it "should call textBlack" $ do
            textBlack `shouldBe` ("\x1b[30m" :: [Char])

        it "should call textSuccess" $ do
            textSuccess `shouldBe` ("\x1b[32m" :: [Char])

        it "should call reset" $ do
            reset `shouldBe` ("\x1b[0m" :: [Char])