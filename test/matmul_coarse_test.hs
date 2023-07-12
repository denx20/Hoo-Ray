import MatMul
import Control.Parallel

main :: IO ()
main = do
  let tmp1 = calculateMatrix 100 1000 1000 806593 145769 1
  let tmp2 = calculateMatrix 100 1000 1000 595865 537770 1
  let tmp3 = calculateMatrix 100 1000 1000 138029 596144 1
  let tmp4 = calculateMatrix 100 1000 1000 687392 142194 1
  let tmp5 = calculateMatrix 100 1000 1000 223510 229806 1
  let tmp6 = calculateMatrix 100 1000 1000 84474 845663 1
  let tmp7 = calculateMatrix 100 1000 1000 54310 132428 1
  let tmp8 = calculateMatrix 100 1000 1000 626498 401610 1
  let tmp9 = calculateMatrix 100 1000 1000 158360 22547 1
  let tmp10 = calculateMatrix 100 1000 1000 234323 572796 1
  print "Done"
