
import Control.Monad.Par
import MatMul (generateRandomMatrix, mmult, getFirstElement)
import Prelude
import System.CPUTime
import Text.Printf

parMmult :: (Int, Int, Int, Double, Int) -> (Int, Int, Int, Double, Int) -> Par Double
parMmult (m1, n1, p1, range1, seed1) (m2, n2, p2, range2, seed2) = do
    a <- spawnP $ generateRandomMatrix m1 n1 range1 seed1
    b <- spawnP $ generateRandomMatrix n1 p1 range1 seed1
    c <- spawnP $ generateRandomMatrix m2 n2 range2 seed2
    d <- spawnP $ generateRandomMatrix n2 p2 range2 seed2
    a' <- get a
    b' <- get b
    c' <- get c
    d' <- get d
    let e = mmult a' b'
    let f = mmult c' d'
    return (getFirstElement e + getFirstElement f)

main :: IO ()
main = do
    start <- getCPUTime
    let pairs = [ (1000, 1000, 1000, 10.0, 672627),
                  (1000, 1000, 1000, 10.0, 276728),
                  (1000, 1000, 1000, 10.0, 448716),
                  (1000, 1000, 1000, 10.0, 800176),
                  (1000, 1000, 1000, 10.0, 494545),
                  (1000, 1000, 1000, 10.0, 138510),
                  (1000, 1000, 1000, 10.0, 876998),
                  (1000, 1000, 1000, 10.0, 222464),
                  (1000, 1000, 1000, 10.0, 845845),
                  (1000, 1000, 1000, 10.0, 60338)]

    let parComputations = runPar $ parMapM (uncurry parMmult) (zip pairs (tail pairs ++ [head pairs]))
    let result = sum parComputations
    print result
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10 ^ 12 :: Double)
    printf "Execution time: %0.6f sec" (diff :: Double)
