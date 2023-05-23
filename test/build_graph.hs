import Control.Monad.State
import Graph
import Language.Haskell.Exts
import System.Environment

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
