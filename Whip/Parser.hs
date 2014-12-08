module Whip.Parser (parse) where

import Whip.Types (Expr(..))
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec hiding (parse)
import Control.Monad (liftM)

parse = P.parse pprogram

pprogram :: Parser [Expr]
pprogram = whitespace >> pexpr `sepEndBy` whitespace

comment :: Parser String
comment = between (char ';') (char '\n') (many $ noneOf "\n")

whitespace :: Parser ()
whitespace = do
    skipMany space
    skipMany (char ',')
    skipMany comment
    return ()

pexpr :: Parser Expr
pexpr = pstring
    <|> pquoted
    <|> pparens
    <|> pnumber
    <|> psymbol where

    symbol :: Parser Char
    symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

    pstring :: Parser Expr
    pstring = liftM (String . read . ("\"" ++) . (++ "\""))
            $ between (char '"') (char '"') (many $ noneOf "\"")

    pquoted :: Parser Expr
    pquoted = do
        char '\''
        x <- pexpr
        return $ Parens [Symbol "quote", x]

    pparens :: Parser Expr
    pparens = do
        char '('
        whitespace
        x <- pexpr `sepEndBy` whitespace
        char ')'
        return $ Parens x

    psymbol :: Parser Expr
    psymbol = do
        x <- letter <|> symbol
        xs <- many (digit <|> letter <|> symbol)
        return $ case x:xs of
            "true"  -> Boolen True
            "false" -> Boolen False
            s       -> Symbol s

    pnumber :: Parser Expr
    pnumber = do
        x <- many1 digit
        return $ Number (read x)
