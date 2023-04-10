-- generate_arithmetic_test.hs
{-# LANGUAGE OverloadedStrings #-}

import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.Text (Text, pack, unpack, intercalate)
import Data.Text.IO (writeFile)
import System.Environment
import Arithmetic

randomFunctionName :: IO String
randomFunctionName = do
    i <- randomRIO (0, 1) :: IO Int
    return $ case i of
        0 -> "sum"
        1 -> "product"

randomArgs :: Int -> Double -> IO [Double]
randomArgs n range = replicateM n (randomRIO (negate range, range) :: IO Double)

randomFunctionCall :: Double -> Int -> IO Text
randomFunctionCall range length = do
    functionName <- randomFunctionName
    args <- randomArgs length range
    let argsText = map (pack . show) args
    return $ pack (functionName ++ " [") <> intercalate ", " argsText <> pack "]"

getNVarNames :: Int -> Text -> [Text]
getNVarNames n prefix = map (\i -> prefix <> pack (show i)) [1..n]

main :: IO ()
main = do
    args <- getArgs
    case length args of 
        3 -> do
            let n = read (args !! 0) :: Int
            let range = read (args !! 1) :: Double
            let length = read (args !! 2) :: Int
            let tmps = getNVarNames n "tmp"
            functionCalls <- replicateM n (randomFunctionCall range length)
            let programText = "import Arithmetic\nimport Prelude hiding (subtract, sum, product)\nimport System.CPUTime\nimport Text.Printf\n\nmain :: IO ()\nmain = do\n  start <- getCPUTime\n" <> mconcat (map (\(i, call) -> "  let " <> tmps !! i <> " = " <> call <> "\n") $ zip [0..] functionCalls) <> "  let result = sum [" <> intercalate ", " tmps <> "]\n  print result\n  end <- getCPUTime\n  let diff = fromIntegral (end - start) / (10 ^ 12 :: Double)\n  printf \"Execution time: %0.6f sec\\n\" (diff :: Double)\n"
            Data.Text.IO.writeFile "Tests/list_test.hs" programText
            putStrLn "list_test.hs has been generated."
        _ -> error "Usage: list_test_gen <nlines> <num_range> <list_length>"
