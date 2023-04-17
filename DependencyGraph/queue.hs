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
import Text.Read hiding (put, get)
import System.Exit
import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable
import Text.Printf
import Data.Time.Clock

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

data Message = Result String String
  deriving (Typeable, Generic)

instance Binary Message

data RemoteCall = RemoteCall String ProcessId (HM.HashMap String String) (HM.HashMap String [String])
  deriving (Show, Typeable, Generic)

instance Binary RemoteCall



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
      let result = fromMaybe "" (HM.lookup fname resultMap) :: String
      send masterPid (Result node result)

      
    handleF :: Process ()
    handleF = do
      -- say $ "HANDLING node " ++ node
      let deps = fromMaybe [] (HM.lookup node depGraph)
      case extractMiddle node of
        "calculateMatrix" -> do
          let vals = map (\x -> deserializeDouble (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = calculateMatrix (vals !! 0) (vals !! 1) (vals !! 2) (vals !! 3) (vals !! 4) (vals !! 5)
          send masterPid (Result node (serializeDouble result))
        "generateRandomMatrix" -> do
          let vals = map (\x -> deserializeDouble (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = generateRandomMatrix (round (vals !! 0)) (round (vals !! 1)) (vals !! 2) (round (vals !! 3))
          send masterPid (Result node (serializeDoubleList result))
        "mmult" -> do
          let vals = map (\x -> deserializeDoubleList (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = mmult (vals !! 0) (vals !! 1)
          send masterPid (Result node (serializeDoubleList result))
        "sumMatrix" -> do
          let vals = map (\x -> deserializeDoubleList (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = sumMatrix (vals !! 0)
          send masterPid (Result node (serializeDouble result))

    handleArgs :: Process ()
    handleArgs = do
      -- say $ "HANDLING node " ++ node
      send masterPid (Result node (serializeDouble (read node :: Double)))

remotable ['remoteCall]

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

dispatchJob :: ProcessId -> NodeId  -> String -> (HM.HashMap String String) -> (HM.HashMap String [String]) -> Process ()
dispatchJob masterId nodeId node resultMap depGraph = do
  let remoteCallClosure = ($(mkClosure 'remoteCall) (RemoteCall node masterId resultMap depGraph))
  _ <- spawn nodeId remoteCallClosure
  liftIO $ putStrLn $ "Sent task to worker: " ++ show nodeId ++ " for node: " ++ node
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

sumVtmpValues :: HM.HashMap String String -> Double
sumVtmpValues hm = HM.foldrWithKey accumulateVtmpValues 0 hm
  where
    accumulateVtmpValues key value acc =
      if "v_tmp" `isPrefixOf` key
        then acc + (deserializeDouble value)
        else acc

    isPrefixOf :: String -> String -> Bool
    isPrefixOf prefix str = take (length prefix) str == prefix


master :: Backend -> String -> [NodeId] -> Process ()
master backend mode workers = do
  start <- liftIO $ getCurrentTime
  say "Master started"
  if mode == "coarse"
    then say "Running coarse-grained version"
    else say "Running fine-grained version"
  if length workers == 0
    then do
      say "No workers available, exiting"
      liftIO $ exitWith (ExitFailure 1)
    else say $ "Master discovered workers: " ++ (show workers)
  dependencyGraph <- liftIO $ buildGraph (if mode == "coarse" then "Tests/matmul_coarse_test.hs" else "Tests/matmul_fine_test.hs")
  let depGraph = HM.fromList dependencyGraph :: HM.HashMap String [String]
  let reversedGraph = reverseDependencyGraph dependencyGraph :: HM.HashMap String [String]
  let indegreeCount = indegreeCounts dependencyGraph :: [(String, Int)]
  let indegreeZeroNodes = map (\(node, cnt) -> node) $ filter (\(v, indegree) -> indegree == 0) indegreeCount :: [String]
  let nodeList = nub $ concatMap (\(n, neighbors) -> n : neighbors) dependencyGraph :: [String]

  workerListVar <- liftIO $ newMVar workers
  queueVar <- liftIO $ newMVar indegreeZeroNodes
  indegreeMapVar <- liftIO $ newMVar $ HM.fromList indegreeCount
  resultsVar <- liftIO $ newMVar HM.empty
  visitVar <- liftIO $ newMVar $ HS.fromList nodeList


  masterPid <- getSelfPid

  let assignJobs :: Process ()
      assignJobs = do
          queue <- liftIO $ readMVar queueVar :: Process [String]
          results <- liftIO $ readMVar resultsVar
          if (null queue && length results == length nodeList)
            then do
              say $ "Final result: " ++ (show (sumVtmpValues results))
              return ()
            else do
              workerList <- liftIO $ readMVar workerListVar :: Process [NodeId]
              let (job, newQueue) = (head queue, tail queue)
              let worker = head workerList
              liftIO $ modifyMVar_ queueVar $ \_ -> return newQueue
              liftIO $ modifyMVar_ workerListVar $ \_ -> return $ rotate workerList
              -- say $ "new node list: " ++ (show (rotate nodeList))
              -- say $ "updated queue: " ++ (show newQueue)
              dispatchJob masterPid worker job results depGraph
              assignJobs
  

  let processReply :: Process ()
      processReply = do
        results <- liftIO $ readMVar resultsVar
        if (length results == length nodeList)
          then return ()
          else do
            Result node result <- expect
            visit <- liftIO $ readMVar visitVar
            -- say $ "Visit set: " ++ (show visit)
            visit' <- liftIO $ modifyMVar visitVar $ \_ -> return (HS.delete node visit, HS.delete node visit)
            -- liftIO $ putStrLn $ "Received result: " ++ node ++ " -> " ++ (show result)
            results <- liftIO $ readMVar resultsVar
            results' <- liftIO $ modifyMVar resultsVar $ \_ -> return (HM.insert node result results, HM.insert node result results)

            -- say $ "Result map: " ++ (show results')
            indegrees <- liftIO $ readMVar indegreeMapVar
            let updatedIndegrees = foldr (updateIndegreeMap node) indegrees (fromMaybe [] $ HM.lookup node reversedGraph)
            liftIO $ modifyMVar_ indegreeMapVar $ \_ -> return updatedIndegrees
            liftIO $ modifyMVar_ queueVar $ \queue ->
              let newNodes = [v | (v, indegree) <- HM.toList updatedIndegrees, indegree == 0, v `notElem` queue, v `elem` visit']
              in do
                return $ queue ++ newNodes
            processReply

  assignJobsAsync <- async (AsyncTask assignJobs)
  processReply
  terminateAllSlaves backend
  end <- liftIO $ getCurrentTime
  say $ "Total time: " ++ (show (diffUTCTime end start))

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
    ["master", host, port, mode] -> do
      backend <- initializeBackend host port myRemoteTable
      startMaster backend (master backend mode)
    ["slave", host, port] -> do
      backend <- initializeBackend host port myRemoteTable
      startSlave backend
    _ -> putStrLn "Invalid command. Use 'master <host> <port> <mode>' or 'slave <host> <port>'."