import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import MatMul (generateRandomMatrix, mmult, sumMatrix)
import Prelude
import System.CPUTime
import Text.Printf


computeSumMatrix :: (Int, Int, Int, Double, Int) -> (Int, Int, Int, Double, Int) -> MVar Double -> IO ()
computeSumMatrix (m1, n1, p1, range1, seed1) (m2, n2, p2, range2, seed2) mvar = do
    let a = generateRandomMatrix m1 n1 range1 seed1
    let b = generateRandomMatrix n1 p1 range1 seed1
    let c = generateRandomMatrix m2 n2 range2 seed2
    let d = generateRandomMatrix n2 p2 range2 seed2
    let e = mmult a b
    let f = mmult c d
    putMVar mvar (sumMatrix e + sumMatrix f)

main :: IO ()
main = do
    start <- getCPUTime
    let pairs = [ (100, 1000, 100, 10.0, 444821),
                  (100, 1000, 100, 10.0, 48929),
                  (100, 1000, 100, 10.0, 87840),
                  (100, 1000, 100, 10.0, 475823),
                  (100, 1000, 100, 10.0, 189244),
                  (100, 1000, 100, 10.0, 122508),
                  (100, 1000, 100, 10.0, 662826),
                  (100, 1000, 100, 10.0, 718531),
                  (100, 1000, 100, 10.0, 923057),
                  (100, 1000, 100, 10.0, 447275)]

    mvars <- mapM (const newEmptyMVar) pairs
    let tasks = zipWith3 computeSumMatrix pairs (tail pairs ++ [head pairs]) mvars
    mapM_ forkIO tasks
    results <- mapM takeMVar mvars
    let result = sum results
    print result
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10 ^ 12 :: Double)
    printf "Execution time: %0.6f sec" (diff :: Double)
