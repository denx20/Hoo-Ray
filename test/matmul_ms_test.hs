import MatMul
import Control.Parallel (par, pseq)

main :: IO ()
main = do
  let tmp1 = calculateMatrix 1000 1000 100 806593 145769 1.0
  let tmp2 = calculateMatrix 1000 1000 100 595865 537770 1.0
  let tmp3 = calculateMatrix 1000 1000 100 138029 596144 1.0
  let tmp4 = calculateMatrix 1000 1000 100 687392 142194 1.0
  let tmp5 = calculateMatrix 1000 1000 100 223510 229806 1.0
  let tmp6 = calculateMatrix 1000 1000 100 84474 845663 1.0
  let tmp7 = calculateMatrix 1000 1000 100 54310 132428 1.0
  let tmp8 = calculateMatrix 1000 1000 100 626498 401610 1.0
  let tmp9 = calculateMatrix 1000 1000 100 158360 22547 1.0
  let tmp10 = calculateMatrix 1000 1000 100 234323 572796 1.0
  let result_list = [tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10]
  let result = foldr1 (\acc x -> x `par` (acc + x)) result_list
  print result
