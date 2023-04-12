import System.Environment
import qualified Graph

main :: IO ()
main = do
  putStrLn "<-- Starting Dependency Graph Generation -->"
  args <- getArgs
  withArgs args Graph.main
  putStrLn "<-- End of Dependency Graph Generation -->"
