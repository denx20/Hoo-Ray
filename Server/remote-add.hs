{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Concurrent (threadDelay)

import System.Environment (getArgs)

import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable
import Text.Printf

remoteAdd :: (Int, Int) -> Process ()
remoteAdd (x, y) = do
  let result = x + y
  say $ printf "Remote addition: %d + %d = %d" x y result


remotable ['remoteAdd]

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

master :: Backend -> [NodeId] -> Process ()
master backend nodes = do
  let remoteNode = head nodes
  say $ printf "Spawning remoteAdd on %s" (show remoteNode)
  pid <- spawn remoteNode ($(mkClosure 'remoteAdd) (5 :: Int, 7 :: Int))
  liftIO $ threadDelay 2000000
  terminateAllSlaves backend

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port myRemoteTable
      startMaster backend (master backend)
    ["slave", host, port] -> do
      backend <- initializeBackend host port myRemoteTable
      startSlave backend
    _ -> putStrLn "Invalid command. Use 'master <host> <port>' or 'slave <host> <port>'."
