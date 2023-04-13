
import Control.Monad.Par
import MatMul
import Prelude
import System.CPUTime
import Text.Printf

parSumMatrix :: (Int, Int, Int, Double, Int, Int) -> (Int, Int, Int, Double, Int, Int) -> Par Double
parSumMatrix (m1, n1, p1, range1, seed1, seed11) (m2, n2, p2, range2, seed2, seed22) = do
    a <- spawnP $ generateRandomMatrix m1 n1 range1 seed1
    b <- spawnP $ generateRandomMatrix n1 p1 range1 seed11
    c <- spawnP $ generateRandomMatrix m2 n2 range2 seed2
    d <- spawnP $ generateRandomMatrix n2 p2 range2 seed22
    a' <- get a
    b' <- get b
    c' <- get c
    d' <- get d
    let e = mmult a' b'
    let f = mmult c' d'
    return (sumMatrix e + sumMatrix f)

main :: IO ()
main = do
    start <- getCPUTime
    let pairs = [ (100, 1000, 100, 10.0, 444821, 24858),
                  (100, 1000, 100, 10.0, 48929, 21437),
                  (100, 1000, 100, 10.0, 87840, 784692),
                  (100, 1000, 100, 10.0, 475823, 743335),
                  (100, 1000, 100, 10.0, 189244, 3684),
                  (100, 1000, 100, 10.0, 122508, 189014),
                  (100, 1000, 100, 10.0, 662826, 384518),
                  (100, 1000, 100, 10.0, 718531, 975455),
                  (100, 1000, 100, 10.0, 923057, 859295),
                  (100, 1000, 100, 10.0, 447275, 154353)]

    let parComputations = runPar $ parMapM (uncurry parSumMatrix) (zip pairs (tail pairs ++ [head pairs]))
    let result = sum parComputations
    print result
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10 ^ 12 :: Double)
    printf "Execution time: %0.6f sec" (diff :: Double)
