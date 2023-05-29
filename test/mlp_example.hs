import MatMul

{-
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
  -}

main :: IO ()
main = do
  {- simulate a 3-layer MLP: R^100 -> R^10 with hidden dimension 2000 -}
  {- data -}
  let x = generateRandomMatrix 100 1 1.0 123456

  {- model weights -}
  let w1 = generateRandomMatrix 20 100 1.0 531631
  let b1 = generateRandomMatrix 20 1 1.0 512512
  let w2 = generateRandomMatrix 20 20 1.0 132456
  let b2 = generateRandomMatrix 20 1 1.0 125135
  let w3 = generateRandomMatrix 10 20 1.0 654321
  let b3 = generateRandomMatrix 10 1 1.0 230330

  {- Parallalizable forward pass -}
  --let h0 = x
  let h01 = upperHalf x
  let h02 = lowerHalf x
  let w11 = leftHalf w1
  let w12 = rightHalf w1
  let tmp11 = mmult w11 h01
  let tmp12 = mmult w12 h02
  let tmp13 = madd tmp11 tmp12
  let tmp1 = madd tmp13 b1
  let h1 = reluMatrix tmp1

  let h11 = upperHalf h1
  let h12 = lowerHalf h1
  let w21 = leftHalf w2
  let w22 = rightHalf w2
  let tmp21 = mmult w21 h11
  let tmp22 = mmult w22 h12
  let tmp23 = madd tmp21 tmp22
  let tmp2 = madd tmp23 b2
  let h2 = reluMatrix tmp2

  let h21 = upperHalf h2
  let h22 = lowerHalf h2
  let w31 = leftHalf w3
  let w32 = rightHalf w3
  let tmp31 = mmult w31 h21
  let tmp32 = mmult w32 h22
  let tmp33 = madd tmp31 tmp32
  let ypred = madd tmp33 b3
  let predsum = sumMatrix ypred
  print predsum

  {- Natural way of forward pass -}
  {-
  let h0_natural = x
  let tmp1_natural = madd (mmult w1 h0_natural) b1
  let h1_natural = reluMatrix tmp1_natural
  let tmp2_natural = madd (mmult w2 h1_natural) b2
  let h2_natural = reluMatrix tmp2_natural
  let y_pred_natural = madd (mmult w3 h2_natural) b3  

  print (sumMatrix (msubtract y_pred y_pred_natural)) -- y_pred and y_pred_other should be the same
  -}

