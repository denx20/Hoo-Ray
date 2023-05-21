import MatMul
import Control.Parallel (par, pseq)
import Data.Time.Clock


upperHalf :: [[a]] -> [[a]]
upperHalf matrix = take halfRows matrix
  where
    halfRows = (length matrix) `div` 2

lowerHalf :: [[a]] -> [[a]]
lowerHalf matrix = drop halfRows matrix
  where
    halfRows = (length matrix) `div` 2

leftHalf :: [[a]] -> [[a]]
leftHalf matrix = map (take halfColumns) matrix
  where
    halfColumns = (length (head matrix)) `div` 2

rightHalf :: [[a]] -> [[a]]
rightHalf matrix = map (drop halfColumns) matrix
  where
    halfColumns = (length (head matrix)) `div` 2

main :: IO ()
main = do
  {- simulate a 3-layer MLP: R^100 -> R^10 with hidden dimension 2000 -}
  {- data -}
  let x = generateRandomMatrix 100 1 1.0 123456

  {- model weights -}
  let w1 = generateRandomMatrix 2000 100 1.0 531631
  let b1 = generateRandomMatrix 2000 1 1.0 512512
  let w2 = generateRandomMatrix 2000 2000 1.0 132456
  let b2 = generateRandomMatrix 2000 1 1.0 125135
  let w3 = generateRandomMatrix 10 2000 1.0 654321
  let b3 = generateRandomMatrix 10 1 1.0 230330

  {- Parallalizable forward pass -}
  let h0 = x
  let h0_1 = upperHalf h0
  let h0_2 = lowerHalf h0
  let w1_1 = leftHalf w1
  let w1_2 = rightHalf w1
  let tmp1_1 = mmult w1_1 h0_1
  let tmp1_2 = mmult w1_2 h0_2
  let tmp1 = madd (madd tmp1_1 tmp1_2) b1
  let h1 = reluMatrix tmp1

  let h1_1 = upperHalf h1
  let h1_2 = lowerHalf h1
  let w2_1 = leftHalf w2
  let w2_2 = rightHalf w2
  let tmp2_1 = mmult w2_1 h1_1
  let tmp2_2 = mmult w2_2 h1_2
  let tmp2 = madd (madd tmp2_1 tmp2_2) b2
  let h2 = reluMatrix tmp2

  let h2_1 = upperHalf h2
  let h2_2 = lowerHalf h2
  let w3_1 = leftHalf w3
  let w3_2 = rightHalf w3
  let tmp3_1 = mmult w3_1 h2_1
  let tmp3_2 = mmult w3_2 h2_2
  let y_pred = madd (madd tmp3_1 tmp3_2) b3
  let pred_sum = sumMatrix y_pred
  print pred_sum

  {- Natural way of forward pass -}
  let h0_natural = x
  let tmp1_natural = madd (mmult w1 h0_natural) b1
  let h1_natural = reluMatrix tmp1_natural
  let tmp2_natural = madd (mmult w2 h1_natural) b2
  let h2_natural = reluMatrix tmp2_natural
  let y_pred_natural = madd (mmult w3 h2_natural) b3  

  print (sumMatrix (msubtract y_pred y_pred_natural)) -- y_pred and y_pred_other should be the same

