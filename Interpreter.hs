import Control.Monad (forever)
import Grammar (eval, Expr, expr)
import Parser (runParser)
import System.IO (hFlush, stdout)


run :: String -> Expr
run = runParser expr

main :: IO ()
main = forever $ do
    putStr "> "
    hFlush stdout -- Force print prompt
    a <- getLine
    print $ eval $ run a
