{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Concurrent.Async (Async, async, wait)
import Control.Monad (forever)
import Data.Binary (Binary, decode, encode)
import Data.ByteString.Lazy (fromStrict, toStrict)
import GHC.Generics (Generic)
import Network.Simple.TCP (connect, recv, send)
import System.Environment (getArgs)

newtype Task = Task Int
  deriving (Generic, Binary)

workerPorts :: [String]
workerPorts = ["9000", "9001", "9002"] -- Add as many ports as required

main :: IO ()
main = do
  args <- getArgs
  case args of
    [value] -> do
      let task = Task (read value)
      threads <- mapM (connectToWorker task) workerPorts
      mapM_ wait threads
    _ -> error "Usage: master <value>"

connectToWorker :: Task -> String -> IO (Async ())
connectToWorker task port = async $ do
  connect "127.0.0.1" port $ \(connectionSocket, remoteAddr) -> do
    putStrLn $ "TCP connection established with " ++ show remoteAddr
    send connectionSocket $ toStrict (encode task)
    forever $ do
      msg <- recv connectionSocket 4096
      case msg of
        Just m -> do
          let result = decode (fromStrict m) :: Int
          print result
        Nothing -> return ()
