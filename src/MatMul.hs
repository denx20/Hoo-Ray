{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module MatMul
  ( generateRandomMatrix,
    matrixBenchmark,
    mmult,
    madd,
    msubtract,
    reluMatrix,
    softmaxByRow,
    maskedSoftmaxByRow,
    getFirstElement,
    sumMatrix,
    calculateMatrix,
    build,
    extractMiddle,
    serializeDouble,
    deserializeDouble,
    serializeDoubleList,
    deserializeDoubleList,
  )
where

import Data.Char (isDigit)
import Data.List
import System.CPUTime
import System.Random (mkStdGen, randomRs)
import Text.Printf

class BuildList a r | r -> a where
  build' :: [a] -> a -> r

instance BuildList a [a] where
  build' l x = reverse $ x : l

instance BuildList a r => BuildList a (a -> r) where
  build' l x y = build' (x : l) y

build :: forall r a. (BuildList a r) => a -> r
build x = build' [] x

{-
Define matrix operations and helper functions for the large matrix workload evaluation
-}
normalize :: [Double] -> [Double]
normalize xs = map (/ sum xs) xs

mmult :: Num a => [[a]] -> [[a]] -> [[a]]
mmult a b = [[sum $ zipWith (*) ar bc | bc <- (transpose b)] | ar <- a]

madd :: Num a => [[a]] -> [[a]] -> [[a]]
madd a b = zipWith (zipWith (+)) a b

msubtract :: Num a => [[a]] -> [[a]] -> [[a]]
msubtract a b = zipWith (zipWith (-)) a b

scaleMatrixByConstant :: Double -> [[Double]] -> [[Double]]
scaleMatrixByConstant constant matrix = map (map (* constant)) matrix

reluMatrix :: (Num a, Ord a) => [[a]] -> [[a]]
reluMatrix matrix = map (map relu) matrix
  where
    relu x = max 0 x

softmaxByRow :: [[Double]] -> [[Double]]
softmaxByRow matrix = map softmax matrix
  where
    softmax row = normalize $ map exp row

maskedSoftmaxByRow :: [[Double]] -> [[Double]] -> [[Double]]
maskedSoftmaxByRow matrix mask = zipWith applyMaskedSoftmax matrix mask
  where
    applyMaskedSoftmax mat_row mask_row = normalize $ zipWith (*) (map exp mat_row) mask_row

matrixBenchmark :: Int -> Int -> Int -> Double -> Int -> IO ()
matrixBenchmark m n p range seed = do
  start <- getCPUTime
  let a = generateRandomMatrix m n range seed
  let b = generateRandomMatrix n p range seed
  let x = mmult a b
  print (getFirstElement x)
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10 ^ 12 :: Double)
  printf "m: %d, n: %d, p: %d. Execution time: %0.6f sec" m n p (diff :: Double)

generateRandomMatrix :: Int -> Int -> Double -> Int -> [[Double]]
generateRandomMatrix m n range seed =
  let gen = mkStdGen seed
      randomsList = randomRs (- range, range) gen
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
extractMiddle ('f' : '_' : rest) = fst $ span (not . isDigit) rest
extractMiddle _ = error "Invalid input format"

readDouble :: String -> Double
readDouble s = read s :: Double

serializeDouble :: Double -> String
serializeDouble x = show x

deserializeDouble :: String -> Double
deserializeDouble = readDouble

serializeDoubleList :: [[Double]] -> String
serializeDoubleList xs = intercalate "\n" $ map (intercalate "," . map show) xs

deserializeDoubleList :: String -> [[Double]]
deserializeDoubleList s = map (map read . splitOn ',') $ lines s

splitOn :: Char -> String -> [String]
splitOn delimiter = foldr (\c acc -> if c == delimiter then "" : acc else (c : head acc) : tail acc) [""]
