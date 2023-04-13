{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Monad (forM_)
import qualified Data.HashMap.Strict as HM
import System.Environment (getArgs)
import Data.List ((\\), nub)

import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable
import Text.Printf

type DependencyGraph = [(String, [String])]

data Message = Result Double
  deriving (Typeable, Generic)

instance Binary Message

data RemoteCall = RemoteCall (String, [String]) ProcessId
  deriving (Show, Typeable, Generic)

instance Binary RemoteCall

remoteCall :: RemoteCall -> Process ()
remoteCall (RemoteCall _ masterPid) = do
  let result = 1 -- TODO: this is a dummy remote call, need to change 
  send masterPid (Result result)



remotable ['remoteCall]

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

{-
executeFunctionAsync :: MVar (HM.HashMap String Int) -> (String, [String]) -> Process ()
executeFunctionAsync resultsVar edge = do
  let closure = $(mkClosure 'remoteCall) (RemoteCall edge)
  myPid <- getSelfPid
  remoteNode <- getSelfNode
  pid <- spawn remoteNode closure
  send pid myPid
  result <- expect :: Process Int
  liftIO $ modifyMVar_ resultsVar $ \hm -> return $ HM.insert (fst edge) result hm
-}

executeFunctionAsync :: Int -> MVar (HM.HashMap String Int) -> (String, [String]) -> Process (Async ())
executeFunctionAsync workerId resultsVar edge = do
  let remoteCallClosure = ($(mkClosure 'remoteCall) (RemoteCall edge))
  remoteNode <- liftIO $ findRemoteNode workerId
  async $ do
    spawn remoteNode remoteCallClosure
    liftIO $ putStrLn $ "Sent task to worker: " ++ show workerId


master :: Backend -> [NodeId] -> Process ()
master backend nodes = do
  let dependencyGraph = [("A", ["B", "C"]), ("B", ["D"]), ("C", ["D"]), ("D", [])]
  let reversedGraph = reverseDependencyGraph dependencyGraph
  let indegreeZeroNodes = findIndegreeZeroNodes reversedGraph

  resultsVar <- liftIO $ newMVar HM.empty

  forM_ indegreeZeroNodes $ \node -> do
    let dependencyEdges = [(v, deps) | (v, deps) <- reversedGraph, node `elem` deps]
    forM_ dependencyEdges $ \edge -> do
      executeFunctionAsync resultsVar edge

  resultMap <- liftIO $ readMVar resultsVar
  say $ "Execution results: " ++ show resultMap
  liftIO $ threadDelay (200 * (10^6))
  terminateAllSlaves backend

slave :: Process ()
slave = do
  remoteCallClosure <- expect
  result <- call remoteCallClosure
  from <- expect :: Process ProcessId
  send from result
  slave


reverseDependencyGraph :: DependencyGraph -> DependencyGraph
reverseDependencyGraph g = concatMap reverseEdges g
  where
    reverseEdges (v, vs) = [(v', [v]) | v' <- vs]

findIndegreeZeroNodes :: DependencyGraph -> [String]
findIndegreeZeroNodes g =
  let allNodes = nub [v | (v, _) <- g]
      nodesWithEdges = nub $ concatMap snd g
   in allNodes \\ nodesWithEdges


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





