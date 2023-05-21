import System.Environment
import Graph(buildGraph, showDataDependencies)

main :: IO ()
main = do
  putStrLn "<-- Starting Dependency Graph Generation -->"
  args <- getArgs
  case args of
      [fileName] -> do
          graph <- buildGraph fileName
          putStr $ showDataDependencies graph
      _ -> error "Usage: dependency-graph <file>"
  putStrLn "<-- End of Dependency Graph Generation -->"
