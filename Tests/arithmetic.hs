-- arithmetic.hs
import System.IO
import System.Random
import Text.Printf
import ArithmeticFunctions (add, mul, sub, divide, pow)

writeArithmeticTest :: Int -> IO ()
writeArithmeticTest n = do
  writeFile "Tests/arithmetic_test.hs" $ unlines
    [ "module Main where"
    , "import Prelude"
    , "import Data.List"
    , "import System.Random"
    , "import ArithmeticFunctions (add, mul, sub, divide, pow)"
    , ""
    , "main :: IO ()"
    , "main = do"
    , "  g <- newStdGen"
    , "  let (n1, g1) = randomRs (1, 100) g :: ([Double], StdGen)"
    , "  let (n2, g2) = randomRs (1, 100) g1 :: ([Double], StdGen)"
    , "  let (ops, _) = randomRs (0, 4) g2 :: ([Int], StdGen)"
    , "  let functions = [add, mul, sub, divide, pow]"
    , "  let results = take " ++ show n ++ " $ zipWith3 (\\op x y -> (functions !! op) x y) ops n1 n2"
    , "  mapM_ (putStrLn . show) results"
    ]

main :: IO ()
main = do
  putStrLn "Enter a positive integer N:"
  n <- readLn :: IO Int
  writeArithmeticTest n
  putStrLn "arithmetic_test.hs has been generated."
