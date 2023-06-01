import MatMul
import Data.List


main :: IO ()
main = do
  {- simulate a transformer layer with input embedding size 256 -}
  {- data -}
  let x = generateRandomMatrix 20 256 1.0 123456
  --let mask = generateRandomMatrix 20 20 1.0 501601

  {- model weights -}
  --let dk = 64
  --let dv = 512

  let wq = generateRandomMatrix 256 64 0.1 654321
  let wk = generateRandomMatrix 256 64 0.1 132456
  let wv = generateRandomMatrix 256 256 0.1 612453

  --let w1 = generateRandomMatrix 256 100 1.0 230340
  --let b1 = generateRandomMatrix 20 100 1.0 671675
  --let w2 = generateRandomMatrix 100 256 1.0 221403
  --let b2 = generateRandomMatrix 20 256 1.0 531631

  {- Forward pass -}
  let q = mmult x wq
  let k = mmult x wk
  let v = mmult x wv
  let kT = transpose k
  let a = mmult q kT
  --let a_scaled = scaleMatrixByConstant 0.125 a
  --let attention_score = maskedSoftmaxByRow a_scaled mask
  --let attention_score = maskedSoftmaxByRow a mask
  let attention_score = softmaxByRow a
  --print attention_score
  let ypred = mmult attention_score v
  --let h0 = mmult attention_score v
  --let h0_sum = madd h0 x
  --let h0_normed = map normalize h0_sum
  --let tmp1_1 = mmult h0_normed w1
  --let tmp11 = mmult h0 w1
  --let tmp12 = madd tmp11 b1
  --let h1 = reluMatrix tmp12
  --let tmp21 = mmult h1 w2
  --let tmp22 = madd tmp21 b2
  --let tmp23 = madd tmp22 h0
  --let ypred = madd tmp22 h0
  --let y_pred = map normalize tmp2_3
  let predsum = sumMatrix ypred
  print predsum
