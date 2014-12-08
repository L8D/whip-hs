module Main (main) where

import Whip.Parser (parse)
import Whip.Types (Expr(..))
import Whip.Interpreter (runProgram)
import Control.Applicative ((<$>))
import qualified Data.Map as M

lmap :: (a -> b) -> Either a c -> Either b c
lmap f = either (Left . f) Right

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither x = maybe (Left x) Right

main :: IO ()
main = do
    contents <- parse "stdin" <$> getContents

    let result = do program <- lmap show contents
                    scope <- runProgram program
                    maybeToEither "cannot find main" (M.lookup "main" scope)

    case result of
        Left err -> error err
        Right (Comput x) -> x >> return ()
        _ -> return ()
