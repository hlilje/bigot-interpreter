import Prelude hiding (lex)
import Lexer (lex)


interpret :: String -> String
interpret src = unlines . map show $ lex src


main :: IO()
main = interact interpret
