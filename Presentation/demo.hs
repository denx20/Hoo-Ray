import Data.Time.Clock (getCurrentTime)

impure1 :: IO UTCTime
impure1 = getCurrentTime

impure2 :: Show a => a -> IO ()
impure2 x = print x

impure3 :: IO String
impure3 = do
    handle <- openFile "file.txt" ReadMode
    hGetContents handle

