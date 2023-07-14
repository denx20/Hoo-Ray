-- generate matmul_ss_test.hs and matmul_ms_test.hs
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Control.Monad (when)
import Data.Text (Text, intercalate, pack)
import Data.Text.IO (writeFile)
import Options.Applicative
  ( Parser,
    auto,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    short,
    switch,
    (<**>),
  )
import System.Random (mkStdGen, randomRs)
import Prelude hiding (writeFile)

{-
Define parameters (l, m, n, p, r) that determine the size of evaluation workload.
The workload consists of the following steps:
1, generate a random matrix of size m * n and a random matrix of size n * p, where every entry of
   the matrices is randomly chosen from the range [-r, r].
2, multiply these two matrices to get a matrix of size m * p, and calculate the sum of all mp entries.
3, Repeat step 1 and 2 for l times and generates l scalar values.
4, Add all $l$ scalar values and return the sum.
-}

data Options = Options
  { testInput :: Bool,
    nlinesInput :: Int,
    mInput :: Int,
    nInput :: Int,
    pInput :: Int,
    rangeInput :: Int
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> switch
      ( long "test"
          <> short 't'
          <> help "Use this flag to generate test files that can be parsed by queue.hs"
      )
    <*> option
      auto
      ( long "nlines"
          <> short 'l'
          <> metavar "NLINES"
          <> help "Number of sets of ops"
      )
    <*> option
      auto
      ( long "m"
          <> short 'm'
          <> metavar "M"
          <> help "Number of rows for the first matrix"
      )
    <*> option
      auto
      ( long "n"
          <> short 'n'
          <> metavar "N"
          <> help "Number of cols for the first matrix (and number of rows for the second)"
      )
    <*> option
      auto
      ( long "p"
          <> short 'p'
          <> metavar "P"
          <> help "Number of cols for the second matrix"
      )
    <*> option
      auto
      ( long "range"
          <> short 'r'
          <> metavar "RANGE"
          <> help "Specify the (-range, range) constraint for each value in the random matrix"
      )

getNVarNames :: Int -> Text -> [Text]
getNVarNames n prefix = map (\i -> prefix <> pack (show i)) [1 .. n]

generateMatrixFunctionCall :: Int -> Int -> Int -> Int -> Text
generateMatrixFunctionCall m n range seed = pack ("generateRandomMatrix " ++ show m ++ " " ++ show n ++ " " ++ show range ++ " " ++ show seed)

generateCalculateMatrixFunctionCall :: Int -> Int -> Int -> Int -> Int -> Int -> Text
generateCalculateMatrixFunctionCall m n p seedA seedB range = pack ("calculateMatrix " ++ show m ++ " " ++ show n ++ " " ++ show p ++ " " ++ show seedA ++ " " ++ show seedB ++ " " ++ show range)

getRandomSeeds :: Int -> Int -> Int -> [Int]
getRandomSeeds n range seed = take n (randomRs (1, range) (mkStdGen seed))

writeProgram :: String -> Text -> IO ()
writeProgram fileName programText = do
  writeFile ("test/" ++ fileName) programText
  putStrLn (fileName ++ " has been generated.")

main :: IO ()
main = do
  options <- execParser opts
  let test = testInput options
  when test $ putStrLn "Generating matmul_test.hs compatible with queue.hs"
  let [nlines, m, n, p, range] = map (\f -> f options) [nlinesInput, mInput, nInput, pInput, rangeInput]
  let seeds = getRandomSeeds (2 * nlines) 1000000 512
  let [as, bs, cs, tmps] = map (getNVarNames nlines) ["a", "b", "c", "tmp"]

  -- Create single thread evaluation program and save to test/matmul_ss_test.hs
  let programText = "import MatMul\nimport Prelude\n\nmain :: IO ()\nmain = do\n" <> mconcat (map (\i -> "  let " <> as !! i <> " = " <> generateMatrixFunctionCall m n range (seeds !! (2 * i)) <> "\n  let " <> bs !! i <> " = " <> generateMatrixFunctionCall n p range (seeds !! (2 * i + 1)) <> "\n  let " <> cs !! i <> " = mmult " <> as !! i <> " " <> bs !! i <> "\n  let " <> tmps !! i <> " = sumMatrix " <> cs !! i <> "\n") [0 .. nlines - 1]) <> "  let result_list = [" <> intercalate ", " tmps <> "]\n  let result = sum result_list\n  print result\n"
  writeProgram "matmul_ss_test.hs" programText

  -- Create SMP evaluation program and save to test/matmul_ms_test.hs
  let concurrentProgramText = "import MatMul\nimport Control.Parallel\n\nmain :: IO ()\nmain = do\n" <> mconcat (map (\i -> "  let " <> tmps !! i <> " = " <> generateCalculateMatrixFunctionCall m n p (seeds !! (2 * i)) (seeds !! (2 * i + 1)) range <> "\n") [0 .. nlines - 1]) <> "  let result_list = [" <> intercalate ", " tmps <> "]\n  let result = foldr1 (\\acc x -> x `par` (acc + x)) result_list\n  print result\n"
  writeProgram "matmul_ms_test.hs" concurrentProgramText

  -- Create coarse and fine grained evaluation program compatible with Queue.hs and save to test/matmul_coarse_test.hs and test/matmul_fine_test.hs
  when test $ do
    let queueProgramText = "import MatMul\n\nmain :: IO ()\nmain = do\n" <> mconcat (map (\i -> "  let " <> tmps !! i <> " = " <> generateCalculateMatrixFunctionCall m n p (seeds !! (2 * i)) (seeds !! (2 * i + 1)) range <> "\n") [0 .. nlines - 1]) <> "  print \"Done\"\n"
    writeProgram "matmul_coarse_test.hs" queueProgramText
    let queueProgramText2 = "import MatMul\n\nmain :: IO ()\nmain = do\n" <> mconcat (map (\i -> "  let " <> as !! i <> " = " <> generateMatrixFunctionCall m n range (seeds !! (2 * i)) <> "\n  let " <> bs !! i <> " = " <> generateMatrixFunctionCall n p range (seeds !! (2 * i + 1)) <> "\n  let " <> cs !! i <> " = mmult " <> as !! i <> " " <> bs !! i <> "\n  let " <> tmps !! i <> " = sumMatrix " <> cs !! i <> "\n") [0 .. nlines - 1]) <> "  print \"Done\"\n"
    writeProgram "matmul_fine_test.hs" queueProgramText2
  where
    opts = info (optionsParser <**> helper) (fullDesc <> progDesc "Generate single-threaded, multi-threaded, and Queue.hs versions of matmul benchmark.")
