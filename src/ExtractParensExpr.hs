module ExtractParensExpr (extractParensExp) where

import Control.Monad.State (MonadState (state), State, runState)
import Language.Haskell.Exts
  ( Exp (App, Paren, Var),
    Name (Ident),
    ParseResult (ParseFailed, ParseOk),
    QName (UnQual),
    SrcSpanInfo,
    noSrcSpan,
    parseExp,
    prettyPrint,
  )

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

extractParensExp :: String -> String
extractParensExp code = do
  let parsed = parseExp code
  case parsed of
    ParseOk ast -> do
      let (ast', (_, exprInfo)) = runState (extractParens ast) (0, [])
      let exprInfo' = reverse exprInfo
      let res = mapM (\(var, expr) -> ["let " ++ var ++ " = " ++ prettyPrint expr]) exprInfo'
      let res_ = unlines (head res) ++ "let res = " ++ prettyPrint ast'
      return $ head res_
    ParseFailed loc err -> return $ head ("Error at " ++ show loc ++ ": " ++ err)

-- main :: IO ()
-- main = do
--   let code = "multiply (add (x * y) (y + z)) (y - z)"
--   let res = extractParensExp code
--   putStrLn res
