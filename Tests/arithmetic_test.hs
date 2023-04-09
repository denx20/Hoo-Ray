module Main where
import Prelude
import Data.List
import System.Random
import ArithmeticFunctions (add, mul, sub, divide, pow)

printTriplets :: (Show a, Show b, Show c) => [a] -> [b] -> [c] -> IO ()
printTriplets xs ys zs = mapM_ printTriplet (zip3 xs ys zs)
  where
    printTriplet (x, y, z) = putStrLn $ show x ++ " " ++ show y ++ " " ++ show z

newtype PlainString = PlainString String
instance Show PlainString where
  show (PlainString s) = s

main :: IO ()
main = do
  putStrLn "Enter a positive integer N:"
  n <- readLn :: IO Int
  writeArithmeticTest n
  putStrLn "arithmetic_test.hs has been generated."
  g <- newStdGen
  let n1 = randomRs (1, 100) g :: ([Double])
  let n2 = randomRs (1, 100) g :: ([Double])
  let ops = randomRs (0, 4) g :: [Int]
  let functions = [add, mul, sub, divide, pow]
  let functionsStr = [PlainString "add", PlainString "mul", PlainString "sub", PlainString "divide", PlainString "pow"]
  printTriplets (map (\i -> functionsStr !! i) ops) n1 n2
  -- let results = take 1000000 $ zipWith3 (\op x y -> (functions !! op) x y) ops n1 n2
  -- mapM_ (putStrLn . show) results
