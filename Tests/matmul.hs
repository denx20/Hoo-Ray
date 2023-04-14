{-# OPTIONS -fglasgow-exts #-}
module MatMul(
     generateRandomMatrix,
     matrixBenchmark,
     mmult,
     getFirstElement,
     sumMatrix,
     calculateMatrix,
     build
) where

import Data.List
import System.CPUTime
import Text.Printf
import Control.Exception
import Control.Monad (replicateM)
import Control.Monad.ST (runST, ST)
import Data.STRef (newSTRef, readSTRef, modifySTRef', STRef)
import System.Random (StdGen, mkStdGen, randomRs)
import Data.Typeable
import Data.Char (isDigit)

class BuildList a r  | r -> a where
    build' :: [a] -> a -> r

instance BuildList a [a] where
    build' l x = reverse$ x:l

instance BuildList a r => BuildList a (a->r) where
    build' l x y = build'(x:l) y

build :: forall r a. (BuildList a r) => a -> r
build x = build' [] x

mmult :: Num a => [[a]] -> [[a]] -> [[a]] 
mmult a b = [ [ sum $ zipWith (*) ar bc | bc <- (transpose b) ] | ar <- a ]

matrixBenchmark :: Int -> Int -> Int -> Double -> Int -> IO()
matrixBenchmark m n p range seed = do
     start <- getCPUTime
     let a = generateRandomMatrix m n range seed
     let b = generateRandomMatrix n p range seed 
     let x = mmult a b
     print (x !! 0 !! 0)
     end <- getCPUTime
     let diff = fromIntegral (end - start) / (10 ^ 12 :: Double)
     printf "m: %d, n: %d, p: %d. Execution time: %0.6f sec" m n p (diff :: Double)

generateRandomMatrix :: Int -> Int -> Double -> Int -> [[Double]]
generateRandomMatrix m n range seed =
    let gen = mkStdGen seed
        randomsList = randomRs (-range, range) gen
    in chunksOf n $ take (m * n) randomsList

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

getFirstElement :: [[a]] -> a
getFirstElement x = head (head x)

sumMatrix :: [[Double]] -> Double
sumMatrix x = sum (map sum x)

calculateMatrix :: Double -> Double -> Double -> Double -> Double -> Double -> Double
calculateMatrix m n p seedA seedB range = sumMatrix (mmult a b)
    where
        m' = round m
        n' = round n
        p' = round p
        seedA' = round seedA
        seedB' = round seedB
        a = generateRandomMatrix m' n' range seedA'
        b = generateRandomMatrix n' p' range seedB'

extractMiddle :: String -> String
extractMiddle ('f':'_':rest) = fst $ span (not . isDigit) rest
extractMiddle _ = error "Invalid input format"

main :: IO ()
main = do
    -- matrixBenchmark 10000 5000 10000 100 512
    -- print $ extractMiddle "f_generateRandomMatrix3428343"
    -- let x = build 1 2 3 4 5 :: [Int]
    print $ calculateMatrix 10.0 1000 100 123.0 512 100
