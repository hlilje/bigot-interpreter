interpret :: String -> String
interpret src = src

main :: IO()
main = interact interpret
