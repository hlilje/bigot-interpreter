module Grammar (eval, Expr, expr) where

import Control.Applicative (Alternative(..))
import Data.Char (isDigit)
import Parser (chainl1, oneOf, Parser, satisfy)


{- Evaluation -}

data Expr
    = Add Expr Expr
    | Mul Expr Expr
    | Sub Expr Expr
    | Lit Int
    deriving Show

eval :: Expr -> Int
eval ex = case ex of
    Add a b -> eval a + eval b
    Mul a b -> eval a * eval b
    Sub a b -> eval a - eval b
    Lit n   -> n


{- Grammar -}

char :: Char -> Parser Char
char c = satisfy (c ==)

string :: String -> Parser String
string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs) }

digit :: Parser Char
digit = satisfy isDigit

natural :: Parser Integer
natural = read <$> some digit

number :: Parser Int
number = do
    s <- string "-" <|> return []
    cs <- some digit
    return $ read (s ++ cs)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

token :: Parser a -> Parser a
token p = do { a <- p; spaces; return a }

reserved :: String -> Parser String
reserved s = token (string s)

parens :: Parser a -> Parser a
parens m = do
    reserved "("
    n <- m
    reserved ")"
    return n

expr :: Parser Expr
expr = term `chainl1` addop

int :: Parser Expr
int = do { n <- number; return (Lit n) }

term :: Parser Expr
term = factor `chainl1` mulop

factor :: Parser Expr
factor = int <|> parens expr

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

addop :: Parser (Expr -> Expr -> Expr)
addop = (infixOp "+" Add) <|> (infixOp "-" Sub)

mulop :: Parser (Expr -> Expr -> Expr)
mulop = infixOp "*" Mul
