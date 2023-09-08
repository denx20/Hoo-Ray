import MatMul

main :: IO ()
main = do
  let a1 = generateRandomMatrix 100 1000 1 806593
  let b1 = generateRandomMatrix 1000 1000 1 145769
  let c1 = mmult a1 b1
  let tmp1 = sumMatrix c1
  let a2 = generateRandomMatrix 100 1000 1 595865
  let b2 = generateRandomMatrix 1000 1000 1 537770
  let c2 = mmult a2 b2
  let tmp2 = sumMatrix c2
  let a3 = generateRandomMatrix 100 1000 1 138029
  let b3 = generateRandomMatrix 1000 1000 1 596144
  let c3 = mmult a3 b3
  let tmp3 = sumMatrix c3
  let a4 = generateRandomMatrix 100 1000 1 687392
  let b4 = generateRandomMatrix 1000 1000 1 142194
  let c4 = mmult a4 b4
  let tmp4 = sumMatrix c4
  let a5 = generateRandomMatrix 100 1000 1 223510
  let b5 = generateRandomMatrix 1000 1000 1 229806
  let c5 = mmult a5 b5
  let tmp5 = sumMatrix c5
  let a6 = generateRandomMatrix 100 1000 1 84474
  let b6 = generateRandomMatrix 1000 1000 1 845663
  let c6 = mmult a6 b6
  let tmp6 = sumMatrix c6
  let a7 = generateRandomMatrix 100 1000 1 54310
  let b7 = generateRandomMatrix 1000 1000 1 132428
  let c7 = mmult a7 b7
  let tmp7 = sumMatrix c7
  let a8 = generateRandomMatrix 100 1000 1 626498
  let b8 = generateRandomMatrix 1000 1000 1 401610
  let c8 = mmult a8 b8
  let tmp8 = sumMatrix c8
  let a9 = generateRandomMatrix 100 1000 1 158360
  let b9 = generateRandomMatrix 1000 1000 1 22547
  let c9 = mmult a9 b9
  let tmp9 = sumMatrix c9
  let a10 = generateRandomMatrix 100 1000 1 234323
  let b10 = generateRandomMatrix 1000 1000 1 572796
  let c10 = mmult a10 b10
  let tmp10 = sumMatrix c10
  print "Done"
