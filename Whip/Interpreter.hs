module Whip.Interpreter (runProgram) where

import Whip.Types (Expr(..), typeOf)
import Control.Monad (foldM)
import qualified Data.Map.Lazy as M

type Scope = M.Map String Expr

library :: Scope
library = M.fromList
    [ "print" ~> Lambda (\(String s) -> Comput $ print s >> return (Parens []))
    , "show" ~> Lambda (String . show)
    ] where (~>) = (,)

runProgram :: [Expr] -> Either String Scope
runProgram = foldM topEval library

topEval :: Scope -> Expr -> Either String Scope
topEval sc (Parens [Symbol "def", Symbol s, e]) = do
    v <- eval sc e
    return $ M.insert s v sc
topEval _ x = Left $ "top level naked expression: " ++ show x

eval :: Scope -> Expr -> Either String Expr
eval s e = case e of
    Parens (x:xs) -> eval s x >>= call s xs
    Symbol v      -> case M.lookup v s of
        Just e    -> Right e
        Nothing   -> Left (show v ++ " is not defined")
    x             -> Right x

call :: Scope -> [Expr] -> Expr -> Either String Expr
call s []     e          = Right e
call s (x:xs) (Lambda f) = eval s x >>= call s xs . f
call _ _      e          = Left (typeOf e ++ " is not a function")
