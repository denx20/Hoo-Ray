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
import Control.Monad (forM_, mapM_, forever, replicateM_, replicateM)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import System.Environment (getArgs)
import Data.List ((\\), nub, delete, intercalate, span, foldl')
import Data.Maybe
import System.Random (randomRIO)
import Data.Hashable (Hashable)
import Graph 
import Data.Char (isDigit)
import MatMul

import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable
import Text.Printf

instance (Binary k, Binary v, Eq k, Hashable k) => Binary (HM.HashMap k v) where
  put hashMap = do
    put (HM.size hashMap)
    forM_ (HM.toList hashMap) $ \(k, v) -> do
      put k
      put v

  get = do
    size <- get
    fmap HM.fromList $ replicateM size $ do
      k <- get
      v <- get
      return (k, v)

data Message = Result String Double
  deriving (Typeable, Generic)

instance Binary Message

data RemoteCall = RemoteCall String ProcessId (HM.HashMap String Double) (HM.HashMap String [String])
  deriving (Show, Typeable, Generic)

instance Binary RemoteCall

extractMiddle :: String -> String
extractMiddle ('f':'_':rest) = fst $ span (not . isDigit) rest
extractMiddle _ = error "Invalid input format"

remoteCall :: RemoteCall -> Process ()
remoteCall (RemoteCall node masterPid resultMap depGraph) = do
  case getPrefix node of
        "v_" -> handleV
        "f_" -> handleF
        _    -> handleArgs
  where
    getPrefix :: String -> String
    getPrefix str = take 2 str

    handleV :: Process ()
    handleV = do
      -- say $ "HANDLING node " ++ node
      let fname = head (fromMaybe [] $ HM.lookup node depGraph)
      let result = fromMaybe (-1) (HM.lookup fname resultMap) :: Double
      send masterPid (Result node result)

      
    handleF :: Process ()
    handleF = do
      -- say $ "HANDLING node " ++ node
      let deps = fromMaybe [] (HM.lookup node depGraph)
      let vals = map (\x -> fromMaybe (-1) (HM.lookup x resultMap)) deps
      case extractMiddle node of
        "calculateMatrix" -> do
          let result = calculateMatrix (vals !! 0) (vals !! 1) (vals !! 2) (vals !! 3) (vals !! 4) (vals !! 5)
          send masterPid (Result node result)

    handleArgs :: Process ()
    handleArgs = do
      -- say $ "HANDLING node " ++ node
      send masterPid (Result node (read node :: Double))

remotable ['remoteCall]

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

dispatchJob :: ProcessId -> NodeId  -> String -> (HM.HashMap String Double) -> (HM.HashMap String [String]) -> Process ()
dispatchJob masterId nodeId node resultMap depGraph = do
  let remoteCallClosure = ($(mkClosure 'remoteCall) (RemoteCall node masterId resultMap depGraph))
  _ <- spawn nodeId remoteCallClosure
  -- liftIO $ putStrLn $ "Sent task to worker: " ++ show nodeId ++ " for node: " ++ node
  return ()

rotate :: [a] -> [a]
rotate [] = []
rotate xs = tail xs ++ [head xs]

indegreeCounts :: DependencyGraph -> [(String, Int)]
indegreeCounts graph = HM.toList $ foldl' updateOutdegree HM.empty graph
  where
    updateOutdegree hm (node, edges) = 
      let updatedHM = HM.insertWith (+) node (length edges) hm
      in foldl' (\acc edge -> HM.insertWith (const id) edge 0 acc) updatedHM edges

sumVtmpValues :: HM.HashMap String Double -> Double
sumVtmpValues hm = HM.foldrWithKey accumulateVtmpValues 0 hm
  where
    accumulateVtmpValues key value acc =
      if "v_tmp" `isPrefixOf` key
        then acc + value
        else acc

    isPrefixOf :: String -> String -> Bool
    isPrefixOf prefix str = take (length prefix) str == prefix


master :: Backend -> [NodeId] -> Process ()
master backend nodes = do
  dependencyGraph <- liftIO $ buildGraph "Tests/matmul_ms_test.hs"
  let depGraph = HM.fromList dependencyGraph :: HM.HashMap String [String]
  let reversedGraph = reverseDependencyGraph dependencyGraph :: HM.HashMap String [String]
  let indegreeCount = indegreeCounts dependencyGraph :: [(String, Int)]
  let indegreeZeroNodes = map (\(node, cnt) -> node) $ filter (\(v, indegree) -> indegree == 0) indegreeCount :: [String]

  nodeListVar <- liftIO $ newMVar nodes
  queueVar <- liftIO $ newMVar indegreeZeroNodes
  indegreeMapVar <- liftIO $ newMVar $ HM.fromList indegreeCount
  resultsVar <- liftIO $ newMVar HM.empty
  visitedVar <- liftIO $ newMVar HS.empty

  say "Master started"
  say $ "Master discovered workers: " ++ (show nodes)
  say $ "Number of workers: " ++ (show (length nodes))
  masterPid <- getSelfPid

  let assignJobs :: Process ()
      assignJobs = do
          queue <- liftIO $ readMVar queueVar :: Process [String]
          if (null queue)
            then do
              say "Queue is empty, waiting for results"
              liftIO $ threadDelay (1 * (10^1))
              results <- liftIO $ readMVar resultsVar :: Process (HM.HashMap String Double)
              say $ "Final result: " ++ (show (sumVtmpValues results))
              return ()
            else do
              nodeList <- liftIO $ readMVar nodeListVar :: Process [NodeId]
              results <- liftIO $ readMVar resultsVar :: Process (HM.HashMap String Double)
              let (job, newQueue) = (head queue, tail queue)
              let worker = head nodeList
              liftIO $ modifyMVar_ queueVar $ \_ -> return newQueue
              liftIO $ modifyMVar_ nodeListVar $ \_ -> return $ rotate nodeList
              -- say $ "new node list: " ++ (show (rotate nodeList))
              -- say $ "updated queue: " ++ (show newQueue)
              dispatchJob masterPid worker job results depGraph
              assignJobs
  

  let processReply :: Process ()
      processReply = do
        maybeResult <- expectTimeout (1 * (10^6))
        case maybeResult of 
            Just (Result node result) -> do
              visited <- liftIO $ readMVar visitedVar
              visited' <- liftIO $ modifyMVar visitedVar $ \_ -> return (HS.insert node visited, HS.insert node visited)
              -- liftIO $ putStrLn $ "Received result: " ++ node ++ " -> " ++ (show result)
              results <- liftIO $ readMVar resultsVar
              results' <- liftIO $ modifyMVar resultsVar $ \_ -> return (HM.insert node result results, HM.insert node result results)

              -- say $ "Result map: " ++ (show results')
              indegrees <- liftIO $ readMVar indegreeMapVar
              let updatedIndegrees = foldr (updateIndegreeMap node) indegrees (fromMaybe [] $ HM.lookup node reversedGraph)
              liftIO $ modifyMVar_ indegreeMapVar $ \_ -> return updatedIndegrees
              liftIO $ modifyMVar_ queueVar $ \queue ->
                let newNodes = [v | (v, indegree) <- HM.toList updatedIndegrees, indegree == 0, v `notElem` queue, v `notElem` visited']
                in do
                  return $ queue ++ newNodes
              processReply
            Nothing -> do
              say "No reply"
              processReply

  assignJobsAsync <- async (AsyncTask assignJobs)
  processReply
  terminateAllSlaves backend

updateIndegreeMap :: String -> String -> HM.HashMap String Int -> HM.HashMap String Int
updateIndegreeMap node v indegrees =
  HM.adjust (\indegree -> indegree - 1) v indegrees

reverseDependencyGraph :: DependencyGraph -> HM.HashMap String [String]
reverseDependencyGraph g = foldr reverseEdges HM.empty g
  where
    reverseEdges (v, vs) revGraph = foldr (\v' acc -> HM.insertWith (++) v' [v] acc) revGraph vs

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