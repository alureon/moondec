module MoonDec where
import Text.Lua
import Control.Lens
import Control.Monad.Writer
import Control.Monad.State

type Output = StateT Int (Writer String)

output :: String -> Output ()
output x = tell x

pushIndent, popIndent :: Output ()
pushIndent = do
    n <- get
    put (n + 4)
popIndent = do
    n <- get
    put (n - 4)

newLine :: Output ()
newLine = do
    output "\n"
    o <- get
    output $ replicate o ' '

decompileFunctionCall :: String -> [Expression] -> Output ()
decompileFunctionCall n args = output n >> if length args == 0
    then output "!"
    else output " " >> decompileExpressionList args

decompileExpressionList :: [Expression] -> Output ()
decompileExpressionList exprs = do
    decompileExpression $ head exprs
    forM_ (tail exprs) $ \e -> output ", " >> decompileExpression e

decompileExpression :: Expression -> Output ()
decompileExpression (Lit a) = output $ show a
decompileExpression (Str s) = output "\"" >> output s >> output "\""
decompileExpression (Boolean b) = case b of
    True -> output "true"
    False -> output "false"
decompileExpression (FunctionCallExpr n args) = decompileFunctionCall n args

decompileStatement :: Statement -> Output ()
decompileStatement (LocalAssign str exp) = do
    output $ str !! 0
    output " = "
    decompileExpression $ exp !! 0

decompileStatement (DoStatement stats) = case head stats of
    LocalAssign vars exprs -> output "with " >> (decompileStatement $ head stats) >> pushIndent >> newLine

decompileStatement (FunctionCallStat n args) = decompileFunctionCall n args

decompile :: Statement -> String
decompile stat = execWriter $ execStateT (decompileStatement stat) 0
-- decompile stat = execWriterT $ decompileStatement stat
-- decompile stat = snd $ runWriter $ decompileStatement stat

