{-# LANGUAGE LambdaCase #-}

module Graph (DependencyGraph, showDataDependencies, extractDataDependencies, buildGraph) where

import Control.Exception (IOException, try)
import Control.Monad.Except
  ( ExceptT (ExceptT),
    MonadError (throwError),
    MonadIO (liftIO),
    forM,
    runExceptT,
  )
import Control.Monad.State
  ( MonadState (get, put),
    State,
    evalState,
  )
import Data.List (find, nub)
import Language.Haskell.Exts
  ( Annotated (ann),
    Binds (BDecls),
    Decl (PatBind),
    Exp (App, Do, InfixApp, Lit, Paren, Var),
    Literal (Frac, Int),
    Module (Module),
    Name (Ident),
    ParseResult (ParseFailed, ParseOk),
    Pat (PVar),
    QName (UnQual),
    QOp (QVarOp),
    Rhs (UnGuardedRhs),
    SrcSpanInfo,
    Stmt (Generator, LetStmt, Qualifier),
    parseModule,
  )

type ErrorMsg = String

buildGraph :: String -> IO (Either ErrorMsg DependencyGraph)
buildGraph fileName = runExceptT $ do
  content <- ExceptT $ readFromFile fileName
  case parseModule content of
    ParseOk ast -> do
      depGraph <- ExceptT . pure $ extractDataDependencies ast
      liftIO . pure $ evalState depGraph 0
    ParseFailed _ errMsg -> throwError errMsg

readFromFile :: FilePath -> IO (Either ErrorMsg String)
readFromFile filePath = do
  result <- try (readFile filePath)
  return $ case result of
    Left ex -> Left $ show (ex :: IOException)
    Right content -> Right content

type DependencyGraph = [(String, [String])]

showDataDependencies :: DependencyGraph -> String
showDataDependencies = unlines . map (\(v, fs) -> "(" ++ show v ++ ", " ++ show fs ++ ")")

handlePatBind :: [String] -> Decl SrcSpanInfo -> State Int DependencyGraph
handlePatBind vars (PatBind _ (PVar _ (Ident _ var)) (UnGuardedRhs _ expr) _) = do
  funcCalls <- collectFunctionCalls vars expr
  uniqueFuncCalls <- forM funcCalls $ \(name, args) ->
    return ("f_" ++ name, args)
  return $ [("v_" ++ var, [name]) | (name, _) <- uniqueFuncCalls] ++ uniqueFuncCalls
handlePatBind _ _ = return []

{-
Use pattern binding to extract the data dependencies from the Abstract Syntax Tree (AST) of the input Haskell program
-}
extractDataDependencies :: Module SrcSpanInfo -> Either String (State Int DependencyGraph)
extractDataDependencies (Module _ _ _ _ decls) = do
  mainDecls <- maybe (Left "No main function found") Right $ findMainFunction decls
  let binds = collectBinds mainDecls
      vars = [v | PatBind _ (PVar _ (Ident _ v)) _ _ <- binds]
  return $ fmap concat . forM binds $ handlePatBind vars
  where
    findMainFunction =
      find
        ( \case
            PatBind _ (PVar _ (Ident _ name)) _ _ -> name == "main"
            _ -> False
        )
    collectBinds (PatBind _ _ (UnGuardedRhs _ (Do _ stmts)) _) = concatMap collectPatBinds stmts
    collectBinds _ = error "Unsupported main function"
    collectPatBinds (Generator _ pat (App _ expr1 expr2)) = [PatBind (ann pat) pat (UnGuardedRhs (ann expr1) expr1) Nothing, PatBind (ann pat) pat (UnGuardedRhs (ann expr2) expr2) Nothing]
    collectPatBinds (Qualifier _ _) = []
    collectPatBinds (LetStmt _ (BDecls _ decls')) = decls'
    collectPatBinds _ = []
extractDataDependencies _ = Left "Unsupported Haskell program"

{-
Analyze AST of a Haskell expression to collect function calls and their arguments.
Return a list of tuples, where each tuple is (function_name, [list of function arguments]) and each function name
is unique (enforced by appending a unique identifier to the end of function name).
-}
collectFunctionCalls :: [String] -> Exp SrcSpanInfo -> State Int [(String, [String])]
collectFunctionCalls vars expr = nub <$> collect expr
  where
    collect e = case e of
      (App {}) -> do
        let (baseFunc, args) = extractFuncAndArgs e
        appResult <- collectApp baseFunc args
        rest <- mapM collect args
        return $ appResult ++ concat rest
      (InfixApp _ e1 op e2) -> do
        c1 <- collect e1
        cop <- collectOp op [e1, e2]
        c2 <- collect e2
        return $ c1 ++ cop ++ c2
      (Paren _ e1) -> collect e1
      _ -> return []

    extractFuncAndArgs (App _ e1 e2) =
      let (f, args) = extractFuncAndArgs e1
       in (f, args ++ [e2])
    extractFuncAndArgs e = (e, [])

    collectFunc s args guard
      | s `notElem` vars = do
          uniqueName <- uniqueFuncName s
          return [(uniqueName, guard args)]
      | otherwise = return []

    collectApp (Var _ (UnQual _ (Ident _ s))) args =
      collectFunc s args (concatMap collectArgs)
    collectApp _ _ = return []

    collectOp op args
      | QVarOp _ (UnQual _ (Ident _ s)) <- op,
        s `notElem` vars = do
          uniqueName <- uniqueFuncName s
          return [(uniqueName, concatMap collectArgs args)]
      | otherwise = return []

    collectArgs (Var _ (UnQual _ (Ident _ v))) = ["v_" ++ v]
    collectArgs (Lit _ (Int _ intVal _)) = [show intVal]
    collectArgs (Lit _ (Frac _ doubleVal _)) = [show (fromRational doubleVal :: Double)]
    collectArgs (Paren _ e) = collectArgs e
    collectArgs _ = []

uniqueFuncName :: String -> State Int String
uniqueFuncName name = do
  counter <- get
  put (counter + 1)
  return $ name ++ show counter
