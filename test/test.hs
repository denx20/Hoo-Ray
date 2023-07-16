import qualified Data.ByteString.UTF8 as UTF8
import MatMul
import System.CPUTime
import Text.Printf

main :: IO ()
main = do
  let m1 = generateRandomMatrix 100 1000 1 806593
  start <- getCPUTime
  let s1 = serializeDoubleList m1
  print $ fromIntegral (UTF8.length $ UTF8.fromString s1) / (1000 ^ 2)
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10 ^ 12)
  printf "Time: %0.3f sec\n" (diff :: Double)
