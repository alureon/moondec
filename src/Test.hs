import System.Exit (exitFailure, exitSuccess)
import Text.Parsec
import Text.Lua

main = do
    c <- readFile "test.lua"
    case parse parseStatement "test.lua" c of
        Left e -> do 
            print e
            exitFailure
        Right expr -> do
            print expr
            exitSuccess
