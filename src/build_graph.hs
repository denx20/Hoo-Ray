import Graph ( buildGraph, showDataDependencies )
import System.Environment ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      buildGraph fileName >>= either error (putStrLn . showDataDependencies)
    _ -> error "Usage: dependency-graph <file>"
