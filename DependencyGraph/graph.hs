{-# LANGUAGE LambdaCase #-}
import Language.Haskell.Exts
import System.Environment
import Data.Maybe
import Data.List
-- import Data.Data
-- import Data.Generics.Uniplate.Data

-- import qualified Data.HashSet as HashSet
import Debug.Trace

module Graph(main) where

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            content <- readFile fileName
            case parseModule content of
                ParseOk ast -> putStrLn $ showDataDependencies $ extractDataDependencies ast
                ParseFailed _ errMsg -> error errMsg
        _ -> error "Usage: dependency-graph <file>"

type DependencyGraph = [(String, [String])]

showDataDependencies :: DependencyGraph -> String
showDataDependencies = unlines . map (\(v, fs) -> "(" ++ show v ++ ", " ++ show fs ++ ")")

extractDataDependencies :: Module SrcSpanInfo -> DependencyGraph
extractDataDependencies (Module _ _ _ _ decls) =
    let mainDecls = fromMaybe (error "No main function found") $ findMainFunction decls
        binds = collectBinds mainDecls
        vars = [v | PatBind _ (PVar _ (Ident _ v)) _ _ <- binds]
    in concatMap (\case
                    (PatBind _ (PVar _ (Ident _ var)) (UnGuardedRhs _ expr) _)
                        -> let funcCalls = collectFunctionCalls vars expr
                        in [(var, [name]) | (name, _) <- funcCalls] ++ funcCalls
                    _ -> []) binds
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

collectFunctionCalls :: [String] -> Exp SrcSpanInfo -> [(String, [String])]
collectFunctionCalls vars expr = nub $ collect expr
  where
    collect e = case e of
                  (App _ e1 e2) ->
                      let (baseFunc, args) = extractFuncAndArgs e
                      in collectApp baseFunc args ++ concatMap collect args
                  (InfixApp _ e1 op e2) -> collect e1 ++ collectOp op e1 e2 ++ collect e2
                  (Paren _ e1) -> collect e1
                  _ -> []

    extractFuncAndArgs (App _ e1 e2) =
        let (f, args) = extractFuncAndArgs e1
        in (f, args ++ [e2])
    extractFuncAndArgs e = (e, [])

    collectApp (Var _ (UnQual _ (Ident _ s))) args
      | s `notElem` vars = [(s, concatMap collectArgs args)]
    collectApp _ _ = []

    collectOp (QVarOp _ (UnQual _ (Ident _ s))) e1 e2
      | s `notElem` vars = [(s, collectArgs e1 ++ collectArgs e2)]
    collectOp _ _ _ = []

    collectArgs (Var _ (UnQual _ (Ident _ v))) = [v]
    collectArgs (Paren _ e) = collectArgs e
    collectArgs _ = []




