module Parser (Parser, runParser) where

import Control.Monad
import Control.Applicative


{- API -}

-- A parser is a function traversing the input character stream and applying its logic
-- over lexemes in order to build a composite data structure for the AST
newtype Parser a = Parser { parse :: String -> [(a, String)] }

-- Run the parser on the string to produce its AST or fail with an error
runParser :: Parser a -> String -> a
runParser m s =
    case parse m s of
        [(res, [])] -> res
        [(_, rs)]   -> error "Parser did not consume entire stream."
        _           -> error "Parser error."


{- Parser Application -}

-- Advance the parser by extracting a single character from the parser stream
item :: Parser Char
item = Parser $ \s ->
    case s of
        []     -> []
        (c:cs) -> [(c, cs)]

-- fmap simply yields a Parser where f is applied to all ASTs resulting from the parse operation
instance Functor Parser where
    fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

-- <*> yields a Parser where the AST (functions) of the first parse operation are applied to
-- the ASTs of the second parse operation, keeping the lexems of the second
instance Applicative Parser where
    pure = return
    (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

-- Create a parser from the given AST
unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])

-- Take one parse operation, map another parse function over its result and flatten
-- the resulting list of lists to compose a new Parser
bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

instance Monad Parser where
    return = unit
    (>>=) = bind


{- Parser Branching -}

-- A failure is represented by a Parser halting reading the stream and returing an empty stream
failure :: Parser a
failure = Parser (\cs -> [])

-- Combines two Parsers by concatenating their results on the same stream
combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

instance MonadPlus Parser where
    mzero = failure
    mplus = combine

-- Combine two optional paths of Parser logic, switching to the result of the second if the
-- first fails with the zero value
option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
    case parse p s of
        []  -> parse q s
        res -> res

instance Alternative Parser where
    empty = mzero
    (<|>) = option
