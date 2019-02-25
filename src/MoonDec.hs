module MoonDec where
import Text.Lua
import Control.Lens
import Control.Monad.Writer

output :: String -> Writer String ()
output x = tell x

decompileExpression :: Expression -> Writer String ()
decompileExpression (Lit a) = output $ show a

decompileStatement :: Statement -> Writer String ()
decompileStatement (LocalAssign str exp) = do
    output $ str !! 0
    output " = "
    decompileExpression $ exp !! 0
decompileStatement (DoStatement stats) = case stats !! 0 of
    LocalAssign vars exprs -> output "with " >> (decompileStatement $ stats !! 0)
decompileStatement (FunctionCallStat n args) = output n >> case length args of
    0 -> output "!"

decompile :: Statement -> String
decompile stat = snd $ runWriter $ decompileStatement stat

