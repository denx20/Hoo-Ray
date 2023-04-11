module MatMul(
     generateRandomMatrix
) where

import Data.List
import System.CPUTime
import Text.Printf
import Control.Exception
import System.Random (randomRIO)
import Control.Monad (replicateM)
import System.Random.Mersenne.Pure64 (newPureMT, randomDouble, PureMT)
import Control.Monad.ST (runST, ST)
import Data.STRef (newSTRef, readSTRef, modifySTRef', STRef)

mmult :: Num a => [[a]] -> [[a]] -> [[a]] 
mmult a b = [ [ sum $ zipWith (*) ar bc | bc <- (transpose b) ] | ar <- a ]

matrixBenchmark :: Int -> Int -> Int -> IO()
matrixBenchmark m n p = do
     start <- getCPUTime
     a <- generateRandomMatrix m n 100
     b <- generateRandomMatrix n p 100
     evaluate(a)
     evaluate(b)
     end <- getCPUTime
     let diff = fromIntegral (end - start) / (10 ^ 12 :: Double)
     printf "m: %d, n: %d, p: %d. Execution time: %0.6f sec" m n p (diff :: Double)

generateRandomMatrix :: Int -> Int -> Double -> IO [[Double]]
generateRandomMatrix m n range = do
    gen <- newPureMT
    return $ runST $ do
        genRef <- newSTRef gen
        replicateM m (replicateM n (randomInRange range genRef))

randomInRange :: Double -> Data.STRef.STRef s PureMT -> ST s Double
randomInRange range genRef = do
    gen <- readSTRef genRef
    let (val, nextGen) = randomDouble gen
        scaledVal = (val * 2 * range) - range
    modifySTRef' genRef (const nextGen)
    return scaledVal


main :: IO ()
main = do
    putStrLn "Enter m:"
    m <- readLn :: IO Int
    putStrLn "Enter n:"
    n <- readLn :: IO Int
    putStrLn "Enter p:"
    p <- readLn :: IO Int

    matrixBenchmark m n p