-- generate matmul_ss_test.hs and matmul_ms_test.hs
{-# LANGUAGE OverloadedStrings #-}

import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.Text (Text, pack, unpack, intercalate)
import Data.Text.IO (writeFile)
import System.Environment
import System.Random (mkStdGen, randomRs)
import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options
  { testInput :: Bool
  , nlinesInput :: Int
  , mInput :: Int
  , nInput :: Int
  , pInput :: Int
  , rangeInput :: Double
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> switch
      ( long "test"
     <> short 't'
     <> help "Use this flag to generate test files that can be parsed by queue.hs" )
  <*> option auto
      ( long "nlines"
     <> short 'l'
     <> metavar "NLINES"
     <> help "Number of sets of ops" )
  <*> option auto
      ( long "m"
     <> short 'm'
     <> metavar "M"
     <> help "Number of rows for the first matrix" )
  <*> option auto
      ( long "n"
     <> short 'n'
     <> metavar "N"
     <> help "Number of cols for the first matrix (and number of rows for the second)" )
  <*> option auto
      ( long "p"
     <> short 'p'
     <> metavar "P"
     <> help "Number of cols for the second matrix" )
  <*> option auto
      ( long "range"
     <> short 'r'
     <> metavar "RANGE"
     <> help "Specify the (-range, range) constraint for each value in the random matrix" )


getNVarNames :: Int -> Text -> [Text]
getNVarNames n prefix = map (\i -> prefix <> pack (show i)) [1..n]

generateMatrixFunctionCall :: Int -> Int -> Double -> Int -> Text
generateMatrixFunctionCall m n range seed = pack ("generateRandomMatrix " ++ show m ++ " " ++ show n ++ " " ++ show range ++ " " ++ show seed)

generateCalculateMatrixFunctionCall :: Int -> Int -> Int -> Int -> Int -> Double -> Text
generateCalculateMatrixFunctionCall m n p seedA seedB range = pack ("calculateMatrix " ++ show m ++ " " ++ show n ++ " " ++ show p ++ " " ++ show seedA ++ " " ++ show seedB ++ " " ++ show range)

getRandomSeeds :: Int -> Int -> Int -> [Int]
getRandomSeeds n range seed = take n (randomRs (1, range) (mkStdGen seed))

main :: IO ()
main = do
    options <- execParser opts
    let test = testInput options
    if test then putStrLn "Generating matmul_test.hs compatible with queue.hs" else return ()
    let nlines = nlinesInput options
    let m = mInput options
    let n = nInput options
    let p = pInput options
    let range = rangeInput options
    let seeds = getRandomSeeds (2*nlines) 1000000 512
    let as = getNVarNames nlines "a"
    let bs = getNVarNames nlines "b"
    let cs = getNVarNames nlines "c"
    let tmps = getNVarNames nlines "tmp"
    let programText = "import MatMul\nimport Prelude\nimport Data.Time.Clock\n\nmain :: IO ()\nmain = do\n" <> mconcat (map (\i -> "  let " <> as !! i <> " = " <> generateMatrixFunctionCall m n range (seeds !! (2*i)) <> "\n  let " <> bs !! i <> " = " <> generateMatrixFunctionCall n p range (seeds !! (2*i+1)) <> "\n  let " <> cs !! i <> " = mmult " <> as !! i <> " " <> bs !! i <> "\n  let " <> tmps !! i <> " = sumMatrix " <> cs !! i <> "\n") [0..nlines-1]) <> "  let result_list = [" <> intercalate ", " tmps <> "]\n  let result = sum result_list\n  print result\n"
    Data.Text.IO.writeFile "Tests/matmul_ss_test.hs" programText
    putStrLn "matmul_ss_test.hs has been generated."
    let concurrentProgramText = "import MatMul\nimport Control.Parallel (par, pseq)\nimport Data.Time.Clock\n\nmain :: IO ()\nmain = do\n" <> mconcat (map (\i -> "  let " <> tmps !! i <> " = " <> generateCalculateMatrixFunctionCall m n p (seeds !! (2*i)) (seeds !! (2*i+1)) range <> "\n") [0..nlines-1]) <> "  let result_list = [" <> intercalate ", " tmps <> "]\n  let result = foldr1 (\\acc x -> x `par` (acc + x)) result_list\n  print result\n"
    Data.Text.IO.writeFile "Tests/matmul_ms_test.hs" concurrentProgramText
    putStrLn "matmul_ms_test.hs has been generated."
    if test then 
        let queueProgramText = "import MatMul\nimport Control.Parallel (par, pseq)\nimport Data.Time.Clock\n\nmain :: IO ()\nmain = do\n" <> mconcat (map (\i -> "  let " <> tmps !! i <> " = " <> generateCalculateMatrixFunctionCall m n p (seeds !! (2*i)) (seeds !! (2*i+1)) range <> "\n") [0..nlines-1]) <> "  print \"Done\"\n"
        in Data.Text.IO.writeFile "Tests/matmul_test.hs" queueProgramText >> putStrLn "matmul_test.hs has been generated."
        else 
            return ()
    where
        opts = info (optionsParser <**> helper) ( fullDesc <> progDesc "Generate single-threaded, multi-threaded, and queue.hs versions of matmul benchmark.")
