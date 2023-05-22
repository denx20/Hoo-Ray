import MatMul
import Data.List


main :: IO ()
main = do
  {- simulate a transformer layer with input embedding size 512 -}
  {- data -}
  let x = generateRandomMatrix 20 512 1.0 123456
  let mask = generateRandomMatrix 20 20 1.0 501601

  {- model weights -}
  let dk = 128
  let dv = 512

  let wq = generateRandomMatrix 512 dk 1.0 654321
  let wk = generateRandomMatrix 512 dk 1.0 132456
  let wv = generateRandomMatrix 512 dv 1.0 612453

  let w1 = generateRandomMatrix dv 2048 1.0 230340
  let b1 = generateRandomMatrix 20 2048 1.0 671675
  let w2 = generateRandomMatrix 2048 512 1.0 221403
  let b2 = generateRandomMatrix 20 512 1.0 531631

  {- Forward pass -}
  let q = mmult x wq
  let k = mmult x wk
  let v = mmult x wv
  let a = mmult q (transpose k)
  let a_scaled = scaleMatrixByConstant (1 / sqrt (fromIntegral dk)) a
  let attention_score = maskedSoftmaxByRow a_scaled mask
  let h0 = madd (mmult attention_score v) x
  let h0_normed = map normalize (h0)
  let tmp1 = madd (mmult h0_normed w1) b1
  let h1 = reluMatrix tmp1
  let tmp2 = madd (mmult h1 w2) b2
  let y_pred = map normalize (madd tmp2 h0)
  let pred_sum = sumMatrix y_pred
  print pred_sum
