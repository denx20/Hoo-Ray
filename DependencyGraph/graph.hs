{-# LANGUAGE LambdaCase #-}
module Graph(main, DependencyGraph, showDataDependencies, extractDataDependencies) where

import Language.Haskell.Exts
import System.Environment
import Data.Maybe
import Data.List
import Debug.Trace
import Control.Monad.State

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            content <- readFile fileName
            case parseModule content of
                ParseOk ast -> putStr $ showDataDependencies $ evalState (extractDataDependencies ast) 0
                ParseFailed _ errMsg -> error errMsg
        _ -> error "Usage: dependency-graph <file>"

type DependencyGraph = [(String, [String])]

showDataDependencies :: DependencyGraph -> String
showDataDependencies = unlines . map (\(v, fs) -> "(" ++ show v ++ ", " ++ show fs ++ ")")

extractDataDependencies :: Module SrcSpanInfo -> State Int DependencyGraph
extractDataDependencies (Module _ _ _ _ decls) = do
    let mainDecls = fromMaybe (error "No main function found") $ findMainFunction decls
        binds = collectBinds mainDecls
        vars = [v | PatBind _ (PVar _ (Ident _ v)) _ _ <- binds]
    fmap concat . forM binds $ \case
                    (PatBind _ (PVar _ (Ident _ var)) (UnGuardedRhs _ expr) _) -> do
                        funcCalls <- collectFunctionCalls vars expr
                        uniqueFuncCalls <- forM funcCalls $ \(name, args) -> do
                            return ("f_" ++ name, args)
                        return $ [("v_" ++ var, [name]) | (name, _) <- uniqueFuncCalls] ++ uniqueFuncCalls
                    _ -> return []

  where
    findMainFunction = find (\case
                            PatBind _ (PVar _ (Ident _ name)) _ _ -> name == "main"
                            _ -> False)
    collectBinds (PatBind _ _ (UnGuardedRhs _ (Do _ stmts)) _) = concatMap collectPatBinds stmts
    collectBinds _ = error "Unsupported main function"
    collectPatBinds (Generator _ pat (App _ expr1 expr2)) = [PatBind (ann pat) pat (UnGuardedRhs (ann expr1) expr1) Nothing, PatBind (ann pat) pat (UnGuardedRhs (ann expr2) expr2) Nothing]
    collectPatBinds (Qualifier _ _) = []
    collectPatBinds (LetStmt _ (BDecls _ decls)) = decls
    collectPatBinds _ = []

collectFunctionCalls :: [String] -> Exp SrcSpanInfo -> State Int [(String, [String])]
collectFunctionCalls vars expr = do
    result <- collect expr
    return $ nub result
  where
    collect e = case e of
                  (App _ e1 e2) -> do
                      let (baseFunc, args) = extractFuncAndArgs e
                      appResult <- collectApp baseFunc args
                      rest <- mapM collect args
                      return $ appResult ++ concat rest
                  (InfixApp _ e1 op e2) -> do
                      c1 <- collect e1
                      cop <- collectOp op e1 e2
                      c2 <- collect e2
                      return $ c1 ++ cop ++ c2
                  (Paren _ e1) -> collect e1
                  _ -> return []

    extractFuncAndArgs (App _ e1 e2) =
        let (f, args) = extractFuncAndArgs e1
        in (f, args ++ [e2])
    extractFuncAndArgs e = (e, [])

    collectApp (Var _ (UnQual _ (Ident _ s))) args
      | s `notElem` vars = do
            uniqueName <- uniqueFuncName s
            return [(uniqueName, concatMap collectArgs args)]
      | otherwise = return []
    collectApp _ _ = return []

    collectOp (QVarOp _ (UnQual _ (Ident _ s))) e1 e2
      | s `notElem` vars = do
            uniqueName <- uniqueFuncName s
            return [(uniqueName, collectArgs e1 ++ collectArgs e2)]
      | otherwise = return []
    collectOp _ _ _ = return []

    collectArgs (Var _ (UnQual _ (Ident _ v))) = ["v_" ++ v]
    collectArgs (Paren _ e) = collectArgs e
    collectArgs _ = []

uniqueFuncName :: String -> State Int String
uniqueFuncName name = do
  counter <- get
  put (counter + 1)
  return $ name ++ show counter




