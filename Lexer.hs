module Lexer (lex) where
import Prelude hiding (lex)


{- Grammar -}

-- data ArithmeticOperator = PLUS | MINUS | MULT | DIV | POWER | SQRT
-- data ComparisonOperator = LESS | GREATER | LESS_EQ | GREATER_EQ | EQUAL | NOT_EQ
-- data LogicOperator = AND | OR
-- data UnaryOperator = NEG | NOT
-- data BinaryOperator = ArithmeticOperator | ComparisonOperator | LogicOperator
-- data Literal = IDENTIFIER | NUMBER | STRING
-- data Separator = DOT | LEFT_PAREN | RIGHT_PAREN | LEFT_BRACE | RIGHT_BRACE
-- data Other = ASSIGN | EOF

-- data TokenType = UnaryOperator | BinaryOperator | Literal | Separator | Other
--                deriving (Enum, Show)
data TokenType = PLUS | MINUS | EOF deriving (Enum, Show)

data Token = Token { tokenType :: TokenType
                   , lexeme :: String
                   , line :: Int } deriving (Show)

{- Lexer -}

addToken :: TokenType -> Token
addToken token = Token { tokenType = token, lexeme = "", line = 0 }

scanToken :: Char -> Token
scanToken c = case c of
    '+' -> addToken PLUS
    '-' -> addToken MINUS
    _   -> addToken EOF

lex :: String -> [Token]
lex source = map scanToken source
