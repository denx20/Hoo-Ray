module MatMul(
     generateRandomMatrix,
     matrixBenchmark,
     mmult,
     getFirstElement,
     sumMatrix,
     calculateMatrix
) where

import Data.List
import System.CPUTime
import Text.Printf
import Control.Exception
import Control.Monad (replicateM)
import Control.Monad.ST (runST, ST)
import Data.STRef (newSTRef, readSTRef, modifySTRef', STRef)
import System.Random (StdGen, mkStdGen, randomRs)

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

calculateMatrix :: Int -> Int -> Int -> Int -> Int -> Double -> IO Double
calculateMatrix m n p seedA seedB range = do
  let a = generateRandomMatrix m n range seedA
  let b = generateRandomMatrix n p range seedB
  let c = mmult a b
  return $ sumMatrix c

main :: IO ()
main = do
    matrixBenchmark 10000 5000 10000 100 512