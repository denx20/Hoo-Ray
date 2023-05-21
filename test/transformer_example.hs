import MatMul
import Control.Parallel (par, pseq)
import Data.Time.Clock


main :: IO ()
main = do
  {- simulate a transformer layer with input embedding size 512 -}
  {- data -}
  let x = generateRandomMatrix 20 512 1.0 123456
  let mask = generateRandomMatrix 20 20 1.0 501601

  {- model weights -}
  let dk = 512
  let dv = 512

  let Wq = generateRandomMatrix 512 dk 1.0 654321
  let Wk = generateRandomMatrix 512 dk 1.0 132456
  let Wv = generateRandomMatrix 512 dv 1.0 612453

  let w1 = generateRandomMatrix dv 2048 1.0 230340
  let b1 = generateRandomMatrix 20 2048 1.0 671675
  let w2 = generateRandomMatrix 2048 512 1.0 221403
  let b2 = generateRandomMatrix 20 512 1.0 531631

  {- Forward pass -}
  let q = mmult x Wq
  let k = mmult x Wk
  let v = mmult x Wv
  let a = mmult q (transpose k)
  let a_scaled = scaleMatrixByConstant 1/dk**0.5 a
  let attention_score = maskedSoftmaxByRow a_scaled mask
  let h0 = mmult attention_score v
  let tmp1 = madd (mmult h0 w1) b1
  let h1 = reluMatrix tmp1
  let y_pred = madd (mmult h1 w2) b2
  let pred_sum = sumMatrix y_pred
  print pred_sum
