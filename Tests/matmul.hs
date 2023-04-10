import Data.List

mmult :: Num a => [[a]] -> [[a]] -> [[a]] 
mmult a b = [ [ sum $ zipWith (*) ar bc | bc <- (transpose b) ] | ar <- a ]

main :: IO ()
main = do
     let a = [[1,2,3],[4,5,6]]
     let b = [[1,2],[3,4],[5,6]]
     print $ mmult a b