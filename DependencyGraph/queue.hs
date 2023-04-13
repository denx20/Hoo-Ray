{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Monad (forM_, mapM_, forever)
import qualified Data.HashMap.Strict as HM
import System.Environment (getArgs)
import Data.List ((\\), nub, delete)
import System.Random (randomRIO)

import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable
import Text.Printf

type DependencyGraph = [(String, [String])]

data Message = Result String String
  deriving (Typeable, Generic)

instance Binary Message

data RemoteCall = RemoteCall String ProcessId
  deriving (Show, Typeable, Generic)

instance Binary RemoteCall

remoteCall :: RemoteCall -> Process ()
remoteCall (RemoteCall node masterPid) = do
  result <- liftIO $ randomString 10
  send masterPid (Result node result)

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
  let dependencyGraph = [("A", ["B", "C"]), ("B", ["D"]), ("C", ["D"]), ("D", [])]
  let reversedGraph = reverseDependencyGraph dependencyGraph
  let indegreeCount = [(v, length vs) | (v, vs) <- dependencyGraph]
  let indegreeZeroNodes = map (\(node, cnt) -> node) $ filter (\(v, indegree) -> indegree == 0) indegreeCount

  nodeListVar <- liftIO $ newMVar nodes
  queueVar <- liftIO $ newMVar indegreeZeroNodes
  indegreeMapVar <- liftIO $ newMVar $ HM.fromList indegreeCount

  say "Master started"
  say $ "Master discovered workers: " ++ (show nodes)
  masterPid <- getSelfPid

  queue <- liftIO $ readMVar queueVar
  say $ "queue " ++ (show queue)
  indegreeMap <- liftIO $ readMVar indegreeMapVar
  say $ "indegreeMap " ++ (show indegreeMap)

  let assignJobs = do
          queue <- liftIO $ readMVar queueVar :: Process [String]
          if null queue
            then return ()
            else do
              nodeList <- liftIO $ readMVar nodeListVar :: Process [NodeId]
              say $ "got node list: " ++ (show nodeList)
              let (job, newQueue) = (head queue, tail queue)
              let worker = head nodeList
              liftIO $ modifyMVar_ queueVar $ \_ -> return newQueue
              liftIO $ modifyMVar_ nodeListVar $ \_ -> return $ rotate nodeList
              dispatchJob masterPid worker job
              assignJobs
          
  -- let processReply = do
  --       Result node result <- expect
  --       liftIO $ putStrLn $ "Received result: " ++ node ++ " -> " ++ result
  --       modifyMVar_ indegreeMapVar $ \indegrees ->
  --         let updatedIndegrees = foldr (updateIndegreeMap node) indegrees (reversedGraph !! (HM.lookupDefault (-1) node indegrees))
  --         in return $! updatedIndegrees
  --       modifyMVar_ queueVar $ \queue ->
  --         let newNodes = [v | (v, indegree) <- HM.toList updatedIndegrees, indegree == 0, v `notElem` queue]
  --         in return $ queue ++ newNodes
  --       processReply

  -- async processReply
  forever assignJobs

  resultMap <- liftIO $ readMVar resultsVar
  say $ "Execution results: " ++ show resultMap
  liftIO $ threadDelay (200 * (10^6))
  terminateAllSlaves backend

updateIndegreeMap :: String -> String -> HM.HashMap String Int -> HM.HashMap String Int
updateIndegreeMap node v indegrees =
  HM.adjust (\indegree -> indegree - 1) v indegrees

reverseDependencyGraph :: DependencyGraph -> DependencyGraph
reverseDependencyGraph g = concatMap reverseEdges g
  where
    reverseEdges (v, vs) = [(v', [v]) | v' <- vs]

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