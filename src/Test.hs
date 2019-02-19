import System.Exit (exitFailure)
import Text.Parsec
import Text.Lua

main = do
    c <- readFile "test.lua"
    case parse parseStatement "test.lua" c of
        Left e -> print e
        Right expr -> print expr
    exitFailure
