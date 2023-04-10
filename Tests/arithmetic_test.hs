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
  g <- newStdGen
  -- let (n1, g1) = randomR (1, 100) g :: ([Double], StdGen)
  -- print (take n rand_doubles)
  -- let (n1, n2) = partition (even . fst) rand_doubles
  let rand_doubles = randomRs (1, 100) g :: ([Double])
  let (x1, x2) = partition (even . fst) (zip [0..] rand_doubles)
  let (_, n1) = unzip x1
  let (_, n2) = unzip x2
  let ops = randomRs (0, 4) g :: [Int]
  let functions = [add, mul, sub, divide, pow]
  let functionsStr = [PlainString "add", PlainString "mul", PlainString "sub", PlainString "divide", PlainString "pow"]
  printTriplets (map (\i -> functionsStr !! i) ops) n1 n2
  -- let results = take 1000000 $ zipWith3 (\op x y -> (functions !! op) x y) ops n1 n2
  -- mapM_ (putStrLn . show) results
