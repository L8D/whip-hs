module Main (main) where

import Test.Hspec

main :: IO ()
main = hspec $ do
    it "should be working?" $ do
        2 `shouldBe` (2 :: Int)
