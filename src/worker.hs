{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Monad (forever)
import Data.Binary (Binary, decode, encode)
import Data.ByteString.Lazy (fromStrict, toStrict)
import GHC.Generics (Generic)
import Network.Simple.TCP
  ( HostPreference (Host),
    recv,
    send,
    serve,
  )
import System.Environment (getArgs)

newtype Task = Task Int
  deriving (Generic, Binary)

double :: Int -> Int
double x = x * 2

main :: IO ()
main = do
  args <- getArgs
  case args of
    [port] -> serve (Host "127.0.0.1") port $ \(connectionSocket, remoteAddr) -> do
      putStrLn $ "TCP connection established from " ++ show remoteAddr
      forever $ do
        msg <- recv connectionSocket 4096
        case msg of
          Just m -> do
            let Task x = decode (fromStrict m) :: Task
            send connectionSocket $ toStrict (encode (double x))
          Nothing -> return ()
    _ -> error "Usage: worker <port>"
