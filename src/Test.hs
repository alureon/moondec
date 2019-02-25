import System.Exit (exitFailure, exitSuccess)
import Text.Parsec
import Text.Lua
import MoonDec

main = do
    c <- readFile "test.lua"
    case parse parseStatement "test.lua" c of
        Left e -> do 
            print e
            exitFailure
        Right stat -> do
            print stat
            print $ decompile stat
            -- print $ runWriter $ decompileTest stat
            exitSuccess
