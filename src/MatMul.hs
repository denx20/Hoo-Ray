{-# LANGUAGE RankNTypes #-}

module MatMul
  ( generateRandomMatrix,
    matrixBenchmark,
    mmult,
    madd,
    msubtract,
    scaleMatrixByConstant,
    normalize,
    reluMatrix,
    softmaxByRow,
    upperHalf,
    lowerHalf,
    leftHalf,
    rightHalf,
    maskedSoftmaxByRow,
    getFirstElement,
    sumMatrix,
    calculateMatrix,
    extractMiddle,
    serializeDouble,
    deserializeDouble,
    serializeDoubleList,
    deserializeDoubleList,
  )
where

import Data.Char (isDigit)
import Data.List (intercalate, transpose)
import System.CPUTime (getCPUTime)
import System.Random (mkStdGen, randomRs)
import Text.Printf (printf)

{-
Define matrix operations and helper functions for the large matrix workload evaluation
-}
normalize :: [Double] -> [Double]
normalize xs = map (/ sum xs) xs

mmult :: (Num a) => [[a]] -> [[a]] -> [[a]]
mmult a b = [[sum $ zipWith (*) ar bc | bc <- transpose b] | ar <- a]

madd :: (Num a) => [[a]] -> [[a]] -> [[a]]
madd = zipWith (zipWith (+))

msubtract :: (Num a) => [[a]] -> [[a]] -> [[a]]
msubtract = zipWith (zipWith (-))

scaleMatrixByConstant :: Double -> [[Double]] -> [[Double]]
scaleMatrixByConstant constant = map (map (* constant))

reluMatrix :: (Num a, Ord a) => [[a]] -> [[a]]
reluMatrix = map (map relu)
  where
    relu = max 0

softmaxByRow :: [[Double]] -> [[Double]]
softmaxByRow = map softmax
  where
    softmax row = normalize $ map exp row

maskedSoftmaxByRow :: [[Double]] -> [[Double]] -> [[Double]]
maskedSoftmaxByRow = zipWith applyMaskedSoftmax
  where
    applyMaskedSoftmax mat_row mask_row = normalize $ zipWith (*) (map exp mat_row) mask_row

upperHalf :: [[Double]] -> [[Double]]
upperHalf matrix = take halfRows matrix
  where
    halfRows = length matrix `div` 2

lowerHalf :: [[Double]] -> [[Double]]
lowerHalf matrix = drop halfRows matrix
  where
    halfRows = length matrix `div` 2

leftHalf :: [[Double]] -> [[Double]]
leftHalf matrix = map (take halfColumns) matrix
  where
    halfColumns = length (head matrix) `div` 2

rightHalf :: [[Double]] -> [[Double]]
rightHalf matrix = map (drop halfColumns) matrix
  where
    halfColumns = length (head matrix) `div` 2

matrixBenchmark :: Int -> Int -> Int -> Double -> Int -> IO ()
matrixBenchmark m n p range seed = do
  start <- getCPUTime
  let a = generateRandomMatrix m n range seed
  let b = generateRandomMatrix n p range seed
  let x = mmult a b
  print (getFirstElement x)
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10 ^ (12 :: Integer) :: Double)
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
extractMiddle ('f' : '_' : rest) = takeWhile (not . isDigit) rest
extractMiddle _ = error "Invalid input format"

readDouble :: String -> Double
readDouble s = read s :: Double

serializeDouble :: Double -> String
serializeDouble = show

deserializeDouble :: String -> Double
deserializeDouble = readDouble

serializeDoubleList :: [[Double]] -> String
serializeDoubleList xs = intercalate "\n" $ map (intercalate "," . map show) xs

deserializeDoubleList :: String -> [[Double]]
deserializeDoubleList s = map (map read . splitOn ',') $ lines s

splitOn :: Char -> String -> [String]
splitOn delimiter = foldr (\c acc -> if c == delimiter then "" : acc else (c : head acc) : tail acc) [""]
