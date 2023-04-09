import System.Environment
import qualified DependencyGraph.Graph as Graph

main :: IO ()
main = do
  args <- getArgs
  withArgs args Graph.main
