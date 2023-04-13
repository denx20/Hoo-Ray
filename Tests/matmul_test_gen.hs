-- generate matmul_test.hs
{-# LANGUAGE OverloadedStrings #-}

import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.Text (Text, pack, unpack, intercalate)
import Data.Text.IO (writeFile)
import System.Environment
import System.Random (newStdGen, randomRs)

getNVarNames :: Int -> Text -> [Text]
getNVarNames n prefix = map (\i -> prefix <> pack (show i)) [1..n]

generateMatrixFunctionCall :: Int -> Int -> Double -> Int -> Text
generateMatrixFunctionCall m n range seed = pack ("generateRandomMatrix " ++ show m ++ " " ++ show n ++ " " ++ show range ++ " " ++ show seed)

getRandomSeeds :: Int -> Int -> IO [Int]
getRandomSeeds n range = do
    gen <- newStdGen
    return $ take n (randomRs (1, range) gen)

main :: IO ()
main = do
    args <- getArgs
    case length args of 
        5 -> do
            let nlines = read (args !! 0) :: Int
            let m = read (args !! 1) :: Int
            let n = read (args !! 2) :: Int
            let p = read (args !! 3) :: Int
            let range = read (args !! 4) :: Double
            seeds <- getRandomSeeds (2*nlines) 1000000
            let as = getNVarNames nlines "a"
            let bs = getNVarNames nlines "b"
            let cs = getNVarNames nlines "c"
            let tmps = getNVarNames nlines "tmp"
            let programText = "import MatMul\nimport Prelude\n\nmain :: IO ()\nmain = do\n" <> mconcat (map (\i -> "  let " <> as !! i <> " = " <> generateMatrixFunctionCall m n range (seeds !! (2*i)) <> "\n  let " <> bs !! i <> " = " <> generateMatrixFunctionCall n p range (seeds !! (2*i+1)) <> "\n  let " <> cs !! i <> " = mmult " <> as !! i <> " " <> bs !! i <> "\n  let " <> tmps !! i <> " = sumMatrix " <> cs !! i <> "\n") [0..nlines-1]) <> "  let result_list = [" <> intercalate ", " tmps <> "]\n  let result = sum result_list\n  print result\n"
            Data.Text.IO.writeFile "Tests/matmul_test.hs" programText
            putStrLn "matmul_test.hs has been generated."
        _ -> error "Usage: matmul_test_gen <nlines> <m> <n> <p> <range>"