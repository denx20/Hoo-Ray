import Graph
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      buildGraph fileName >>= either putStrLn (putStrLn . showDataDependencies)
    _ -> error "Usage: dependency-graph <file>"
