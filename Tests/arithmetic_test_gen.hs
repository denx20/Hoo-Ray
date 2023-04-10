-- generate_arithmetic_test.hs
{-# LANGUAGE OverloadedStrings #-}

import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.Text (Text, pack, unpack)
import Data.Text.IO (writeFile)
import Arithmetic

randomFunctionName :: IO String
randomFunctionName = do
    i <- randomRIO (0, 4) :: IO Int
    return $ case i of
        0 -> "add"
        1 -> "multiply"
        2 -> "subtract"
        3 -> "divide"
        4 -> "exponentiate"

randomArgs :: Int -> IO [Double]
randomArgs n = replicateM n (randomRIO (-10, 10))

randomFunctionCall :: IO Text
randomFunctionCall = do
    functionName <- randomFunctionName
    args <- randomArgs 2
    let argsText = map (pack . show) args
    return $ pack (functionName ++ " ") <> mconcat (map (\a -> "(" <> a <> ")" <> " ") argsText)

main :: IO ()
main = do
    putStrLn "Enter the number of lines (N):"
    n <- readLn :: IO Int
    functionCalls <- replicateM n randomFunctionCall
    let programText = "import Arithmetic\nimport Prelude hiding (subtract)\nimport System.CPUTime\nimport Text.Printf\n\nmain :: IO ()\nmain = do\n  start <- getCPUTime\n" <> mconcat (map (\call -> "  let _ = " <> call <> "\n") functionCalls) <> "  end <- getCPUTime\n  let diff = fromIntegral (end - start) / (10 ^ 12 :: Double)\n  printf \"Execution time: %0.6f sec\\n\" (diff :: Double)\n"
    Data.Text.IO.writeFile "Tests/arithmetic_test.hs" programText
    putStrLn "arithmetic_test.hs has been generated."
