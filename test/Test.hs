module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Whip

main :: IO ()
main = hspec $ do
    describe "Expr" $ do
        it "should compare equivalent strings" $
            property $ \s -> String s == String s
