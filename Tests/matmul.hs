module MatMul(
     generateRandomMatrix
) where

import Data.List
import System.CPUTime
import Text.Printf
import Control.Exception
import Control.Monad (replicateM)
import System.Random.Mersenne.Pure64 (newPureMT, randomDouble, PureMT)
import Control.Monad.ST (runST, ST)
import Data.STRef (newSTRef, readSTRef, modifySTRef', STRef)
import System.Random (StdGen, mkStdGen, randomRs)

mmult :: Num a => [[a]] -> [[a]] -> [[a]] 
mmult a b = [ [ sum $ zipWith (*) ar bc | bc <- (transpose b) ] | ar <- a ]

matrixBenchmark :: Int -> Int -> Int -> IO()
matrixBenchmark m n p = do
     start <- getCPUTime
     let a = generateRandomMatrix m n 100 512
     let b = generateRandomMatrix n p 100 512
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

main :: IO ()
main = do
    matrixBenchmark 10000 5000 10000