{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Concurrent (threadDelay)
import MatMul
import System.CPUTime

import System.Environment (getArgs)

import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable
import Text.Printf

data Message = Result Double
  deriving (Typeable, Generic)

instance Binary Message

remoteAdd :: (Int, Int) -> Process ()
remoteAdd (x, y) = do
  let result = x + y
  say $ printf "Remote addition: %d + %d = %d" x y result

remoteGenerateMatrix :: (Int, Int, Int, ProcessId) -> Process ()
remoteGenerateMatrix (m, n, p, from) = do
     let a = generateRandomMatrix m n 100 512
     let b = generateRandomMatrix n p 100 512
     let x = mmult a b
     say $ show (x !! 0 !! 0)
     send from (Result (x !! 0 !! 0))


remotable ['remoteAdd, 'remoteGenerateMatrix]

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

master :: Backend -> [NodeId] -> Process ()
master backend nodes = do
  let remoteNode = head nodes
  myPid <- getSelfPid

  say $ printf "Spawning remoteGenerateMatrix on %s" (show remoteNode)
  -- pid <- spawn remoteNode ($(mkClosure 'remoteAdd) (5 :: Int, 7 :: Int))
  pid <- spawn remoteNode ($(mkClosure 'remoteGenerateMatrix) (10000 :: Int, 100 :: Int, 10000 :: Int, myPid))
  Result x <- expect
  say (show x)
  liftIO $ threadDelay (200 * (10^6))
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
