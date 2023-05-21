import Data.Time.Clock (getCurrentTime)

impure1 :: IO UTCTime
impure1 = getCurrentTime

impure2 :: Show a => a -> IO ()
impure2 x = print x

impure3 :: IO String
impure3 = do
    handle <- openFile "file.txt" ReadMode
    hGetContents handle

pure1 :: Int -> Int -> Int
pure1 a b = a * b

pure2 :: Double
pure2 = 3.14

main :: IO ()
main = do
    result1 <- impure1
    let result2 = pure1 10 20

-- Pure functions
subtract :: (Num a) => a -> a -> a
map :: (a -> b) -> [a] -> [b]