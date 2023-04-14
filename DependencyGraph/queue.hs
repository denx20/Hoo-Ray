{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.Async (race_)
import Control.Distributed.Process.Async 
import Control.Monad (forM_, mapM_, forever, replicateM_)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import System.Environment (getArgs)
import Data.List ((\\), nub, delete, intercalate)
import Data.Maybe
import System.Random (randomRIO)
import Graph 

import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable
import Text.Printf

data Message = Result String String
  deriving (Typeable, Generic)

instance Binary Message

data RemoteCall = RemoteCall String ProcessId
  deriving (Show, Typeable, Generic)

instance Binary RemoteCall

remoteCall :: RemoteCall -> Process ()
remoteCall (RemoteCall node masterPid) = do
  -- say $ "Received task from master: " ++ node
  -- liftIO $ putStrLn $ "Sup bro result to master: " ++ node
  -- liftIO $ putStrLn $ "Received task from master: " ++ node

  result <- liftIO $ randomString 10
  send masterPid (Result node "result")
  -- say $ "Sent result to master: " ++ (show masterPid) ++ " " ++ node ++ " " ++ result

randomString :: Int -> IO String
randomString len = sequence [randomRIO ('a', 'z') | _ <- [1..len]]

remotable ['remoteCall]

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

dispatchJob :: ProcessId -> NodeId  -> String -> Process ()
dispatchJob masterId nodeId node = do
  let remoteCallClosure = ($(mkClosure 'remoteCall) (RemoteCall node masterId))
  _ <- spawn nodeId remoteCallClosure
  liftIO $ putStrLn $ "Sent task to worker: " ++ show nodeId

rotate :: [a] -> [a]
rotate [] = []
rotate xs = tail xs ++ [head xs]

master :: Backend -> [NodeId] -> Process ()
master backend nodes = do
  let dependencyGraph = [("A", ["B", "C"]), ("B", ["D"]), ("C", ["D"]), ("D", []), ("E", [])] :: DependencyGraph
  let reversedGraph = reverseDependencyGraph dependencyGraph :: DependencyGraph
  let indegreeCount = [(v, length vs) | (v, vs) <- dependencyGraph] :: [(String, Int)]
  let indegreeZeroNodes = map (\(node, cnt) -> node) $ filter (\(v, indegree) -> indegree == 0) indegreeCount :: [String]

  nodeListVar <- liftIO $ newMVar nodes
  queueVar <- liftIO $ newMVar indegreeZeroNodes
  indegreeMapVar <- liftIO $ newMVar $ HM.fromList indegreeCount
  resultsVar <- liftIO $ newMVar HM.empty
  visitedVar <- liftIO $ newMVar HS.empty

  say "Master started"
  say $ "Master discovered workers: " ++ (show nodes)
  masterPid <- getSelfPid

  queue <- liftIO $ readMVar queueVar
  say $ "queue " ++ (show queue)
  indegreeMap <- liftIO $ readMVar indegreeMapVar
  say $ "indegreeMap " ++ (show indegreeMap)

  let assignJobs :: Process ()
      assignJobs = do
          queue <- liftIO $ readMVar queueVar :: Process [String]

          if (null queue)
            then do
              -- say "empty queue"
              liftIO $ threadDelay (1 * (10^6))
              assignJobs
            else do
              -- say "Assigning jobs (non-empty queue)"
              nodeList <- liftIO $ readMVar nodeListVar :: Process [NodeId]
              let (job, newQueue) = (head queue, tail queue)
              let worker = head nodeList
              liftIO $ modifyMVar_ queueVar $ \_ -> return newQueue
              liftIO $ modifyMVar_ nodeListVar $ \_ -> return $ rotate nodeList
              -- say $ "new node list: " ++ (show (rotate nodeList))
              -- say $ "updated queue: " ++ (show newQueue)
              dispatchJob masterPid worker job
              liftIO $ threadDelay (1 * (10^6))
              assignJobs
  

  let processReply :: Process ()
      processReply = do
        Result node result <- expect
        visited <- liftIO $ readMVar visitedVar
        visited' <- liftIO $ modifyMVar visitedVar $ \_ -> return (HS.insert node visited, HS.insert node visited)
        liftIO $ putStrLn $ "Received result: " ++ node ++ " -> " ++ result
        indegrees <- liftIO $ readMVar indegreeMapVar
        let updatedIndegrees = foldr (updateIndegreeMap node) indegrees (fromMaybe [] $ lookup node reversedGraph)
        -- liftIO $ putStrLn (showDataDependencies reversedGraph)
        -- liftIO $ putStrLn $ "MaybeGraph: " ++ (intercalate ", " (fromMaybe [] $ lookup node reversedGraph))
        liftIO $ modifyMVar_ indegreeMapVar $ \_ -> return updatedIndegrees
        liftIO $ modifyMVar_ queueVar $ \queue ->
          let newNodes = [v | (v, indegree) <- HM.toList updatedIndegrees, indegree == 0, v `notElem` queue, v `notElem` visited']
          in do
            putStrLn $ "new nodes: " ++ (show newNodes)
            putStrLn $ "new indegrees: " ++ (show updatedIndegrees)
            return $ queue ++ newNodes
        processReply

  assignJobsAsync <- async (AsyncTask assignJobs)
  forever processReply
  -- processReplyAsync <- async (AsyncTask processReply)
  -- x <- async (AsyncTask p)
  -- forever assignJobs

  -- resultMap <- liftIO $ readMVar resultsVar
  -- say $ "Execution results: " ++ show resultMap
  liftIO $ threadDelay (200 * (10^6))
  terminateAllSlaves backend

updateIndegreeMap :: String -> String -> HM.HashMap String Int -> HM.HashMap String Int
updateIndegreeMap node v indegrees =
  HM.adjust (\indegree -> indegree - 1) v indegrees

reverseDependencyGraph :: DependencyGraph -> DependencyGraph
reverseDependencyGraph g = HM.toList $ foldr reverseEdges HM.empty g
  where
    reverseEdges (v, vs) revGraph = foldr (\v' acc -> HM.insertWith (++) v' [v] acc) revGraph vs

-- Takes indegreeMap and returns list of nodes with indegree 0
-- findIndegreeZeroNodes :: HashMap -> [String]
-- findIndegreeZeroNodes indegreeMap = HM.keys $ HM.filter (== 0) indegreeMap

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