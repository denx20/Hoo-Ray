-- REFERENCE ONLY - DO NOT USE
import Data.Generics (Data, mkQ)
import Data.Generics.Schemes (everything)
import Language.Haskell.Exts
import Data.List (nub)
import qualified Data.Maybe

type FunctionName = String
type DependencyGraph = [(FunctionName, [FunctionName])]

main :: IO ()
main = do
    let inputHaskell = "test.hs"
    result <- parseFile inputHaskell

    case result of
        ParseOk ast -> do
            let mainDeps = extractMainDependencies ast
            putStrLn "Data flow in the main function:"
            mapM_ print mainDeps
        ParseFailed loc err -> putStrLn $ "Parse error at " ++ show loc ++ ": " ++ err

extractMainDependencies :: Module SrcSpanInfo -> DependencyGraph
extractMainDependencies (Module _ _ _ _ decls) =
    let mainDeps = concatMap mainDependencies decls
        allFunctions = everything (++) ([] `mkQ` collectFunctionNames) decls
    in nub $ expandDependencies allFunctions mainDeps
  where
    mainDependencies :: Decl SrcSpanInfo -> DependencyGraph
    mainDependencies (FunBind _ matches) =
        case [e | Match _ (Ident _ "main") _ (UnGuardedRhs _ e) _ <- matches] of
            (mainExp:_) -> extractDataFlow mainExp
            _ -> []
    mainDependencies _ = []

    collectFunctionNames :: Decl SrcSpanInfo -> [FunctionName]
    collectFunctionNames (FunBind _ matches) = [name | Match _ (Ident _ name) _ _ _ <- matches]
    collectFunctionNames _ = []

expandDependencies :: [FunctionName] -> DependencyGraph -> DependencyGraph
expandDependencies allFunctions deps = [(f, nub $ depsOf f directDeps) | f <- allFunctions, let directDeps = Data.Maybe.fromMaybe [] (lookup f deps)]
  where
    depsOf f ds = f : concatMap (\d -> maybe [] (depsOf d) (lookup d deps)) ds

extractDataFlow :: Exp SrcSpanInfo -> DependencyGraph
extractDataFlow = everything (++) ([] `mkQ` collectFunctionCalls)
  where
    collectFunctionCalls :: Exp SrcSpanInfo -> DependencyGraph
    collectFunctionCalls (App _ (Var _ (UnQual _ (Ident _ fName))) arg) = [(fName, collectFunctionNames arg)]
    collectFunctionCalls _ = []

    collectFunctionNames :: Exp SrcSpanInfo -> [FunctionName]
    collectFunctionNames (Var _ (UnQual _ (Ident _ name))) = [name]
    collectFunctionNames _ = []