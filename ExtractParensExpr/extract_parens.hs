import Data.Generics
import Language.Haskell.Exts
import Control.Monad.State
import System.Environment (getArgs)

type VarName = String
type ExprInfo l = [(VarName, Exp l)]

extractParens :: Exp SrcSpanInfo -> State (Int, ExprInfo SrcSpanInfo) (Exp SrcSpanInfo)
extractParens e = case e of
  Paren _ e1 -> do
    e1' <- extractParens e1
    var <- state (\(i, info) -> let var = "tmp" ++ show i in (var, (i + 1, (var, e1') : info)))
    return $ Var noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan var))
  App _ e1 e2 -> do
    e1' <- extractParens e1
    e2' <- extractParens e2
    return $ App noSrcSpan e1' e2'
  _ -> return e

main :: IO ()
main = do
  let code = "multiply (add (x * y) (y + z)) (y - z)"
  let parsed = parseExp code
  case parsed of
    ParseOk ast -> do
      let (ast', (_, exprInfo)) = runState (extractParens ast) (0, [])
      let exprInfo' = reverse exprInfo
      forM_ exprInfo' $ \(var, expr) -> do
        putStrLn $ var ++ " = " ++ prettyPrint expr
      putStrLn $ prettyPrint ast'
    ParseFailed loc err -> putStrLn $ "Error at " ++ show loc ++ ": " ++ err

-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     [fileName] -> do
--       result <- parseExpressionsFromFile fileName
--       case result of
--         Left err -> putStrLn $ "Error: " ++ show err
--         Right expressions -> print expressions
--     _ -> putStrLn "Usage: ./expression-parser <filename>"