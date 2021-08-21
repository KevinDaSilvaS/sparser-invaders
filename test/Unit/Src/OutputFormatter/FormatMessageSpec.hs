module Unit.Src.OutputFormatter.FormatMessageSpec where

import Test.Hspec 
import OutputFormatter.Colors
import OutputFormatter.FormatMessage
        
formatMessageSpec :: IO ()
formatMessageSpec = hspec $ do
    describe "OutputFormatter.FormatMessage - Success" $ do
        let msg  = "msg" 
        it "should call _error" $ do
            let expectedOutput = 
                    backgroundError ++ " [Error] " 
                    ++ reset ++ " " ++ textError ++ " " 
                    ++ show msg ++ " " ++ reset
            _error msg `shouldBe` (expectedOutput :: [Char])

        it "should call _warning" $ do
            let expectedOutput = 
                    backgroundWarning ++ " [Warning] "
                    ++ reset ++ " " ++ textWarning ++ " " 
                    ++ show msg ++ " " ++ reset
            _warning msg `shouldBe` (expectedOutput :: [Char])

        it "should call _success" $ do
            let expectedOutput = 
                    backgroundSuccess ++ " [Success] "
                    ++ reset ++ " " ++ textSuccess ++ " " 
                    ++ show msg ++ " " ++ reset
            _success msg `shouldBe` (expectedOutput :: [Char])

        it "should call _info" $ do
            let expectedOutput = textInfo ++ " " ++ show msg ++ " " ++ reset
            _info msg `shouldBe` (expectedOutput :: [Char])

        it "should call _backgroundedInfo" $ do
            let expectedOutput = backgroundInfo ++ msg ++ reset
            _backgroundedInfo msg `shouldBe` (expectedOutput :: [Char])
    
 