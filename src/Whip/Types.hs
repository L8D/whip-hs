module Whip.Types (Expr(..), typeOf) where

data Expr
    = String String
    | Number Integer
    | Symbol String
    | Lambda (Expr -> Expr)
    | Parens [Expr]
    | Boolen Bool
    | Comput (IO Expr)

instance Show Expr where
    show e = case e of
        String s -> show s
        Number n -> show n
        Symbol s -> s
        Lambda _ -> "λ"
        Parens x -> "(" ++ unwords (map show x) ++ ")"
        Boolen b -> if b then "true" else "false"
        Comput _ -> "⊥"

instance Eq Expr where
    String s == String t = s == t
    Number n == Number m = n == m
    Symbol s == Symbol t = s == t
    Parens x == Parens y = x == y
    Boolen b == Boolen c = b == c
    _        == _        = False

typeOf :: Expr -> String
typeOf e = case e of
    String _ -> "string"
    Number _ -> "number"
    Symbol _ -> "symbol"
    Lambda _ -> "lambda"
    Parens _ -> "list"
    Boolen _ -> "bool"
    Comput _ -> "bottom"
