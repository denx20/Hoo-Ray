{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- import Control.Concurrent.MVar
--   ( MVar,
--     modifyMVar_,
--     newMVar,
--     readMVar,
--   )

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
  ( TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTVarIO,
  )
import Control.Distributed.Process
  ( NodeId,
    Process,
    ProcessId,
    RemoteTable,
    expect,
    getSelfPid,
    liftIO,
    say,
    send,
    spawn,
    spawnLocal,
  )
import Control.Distributed.Process.Async
  ( AsyncTask (AsyncTask),
    async,
    cancel,
  )
import Control.Distributed.Process.Backend.SimpleLocalnet
  ( Backend,
    initializeBackend,
    startMaster,
    startSlave,
    terminateAllSlaves,
  )
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process.Internal.CQueue ()
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Monad (unless, when)
import Data.Binary (Binary (get, put))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.List (foldl', transpose)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Graph (DependencyGraph, buildGraph)
import MatMul
  ( calculateMatrix,
    deserializeDouble,
    deserializeDoubleList,
    extractMiddle,
    generateRandomMatrix,
    leftHalf,
    lowerHalf,
    madd,
    mmult,
    reluMatrix,
    rightHalf,
    serializeDouble,
    serializeDoubleList,
    softmaxByRow,
    sumMatrix,
    upperHalf,
  )
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)

instance (Binary k, Binary v, Hashable k) => Binary (HM.HashMap k v) where
  put = put . HM.toList
  get = HM.fromList <$> get

data Message = Result NodeId String String -- MODIFIED
  deriving (Typeable, Generic)

instance Binary Message

data RemoteCall = RemoteCall NodeId String ProcessId (HM.HashMap String String) (HM.HashMap String [String]) -- MODIFIED
  deriving (Show, Typeable, Generic)

instance Binary RemoteCall

{-
Define the remote call used by master to send tasks over to remote workers
-}
remoteCall :: RemoteCall -> Process ()
remoteCall (RemoteCall nodeId node masterPid resultMap depGraph) = do
  case getPrefix node of
    "v_" -> handleV
    "f_" -> handleF
    _ -> handleArgs
  where
    getPrefix :: String -> String
    getPrefix = take 2

    handleV :: Process ()
    handleV = do
      say $ "HANDLING node " ++ node
      let fname = head (fromMaybe [] $ HM.lookup node depGraph)
      let result = fromMaybe "" (HM.lookup fname resultMap) :: String
      send masterPid (Result nodeId node result) -- MODIFIED

    {-
    Handle function execution. Define the task for the remote worker to perform for each type of operation.
    Currently supports the following operations: "calculateMatrix", "generateRandomMatrix", "mmult", and "sumMatrix"
    -}
    handleF :: Process ()
    handleF = do
      say $ "HANDLING node " ++ node
      let deps = fromMaybe [] (HM.lookup node depGraph)
      case extractMiddle node of
        "calculateMatrix" -> do
          let vals = map (\x -> deserializeDouble (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = calculateMatrix (vals !! 0) (vals !! 1) (vals !! 2) (vals !! 3) (vals !! 4) (vals !! 5)
          send masterPid (Result nodeId node (serializeDouble result))
        "generateRandomMatrix" -> do
          let vals = map (\x -> deserializeDouble (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = generateRandomMatrix (round (vals !! 0)) (round (vals !! 1)) (vals !! 2) (round (vals !! 3))
          send masterPid (Result nodeId node (serializeDoubleList result))
        "mmult" -> do
          let vals = map (\x -> deserializeDoubleList (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = mmult (vals !! 0) (vals !! 1)
          send masterPid (Result nodeId node (serializeDoubleList result))
        "sumMatrix" -> do
          let vals = map (\x -> deserializeDoubleList (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = sumMatrix (vals !! 0)
          send masterPid (Result nodeId node (serializeDouble result))
        "madd" -> do
          let vals = map (\x -> deserializeDoubleList (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = madd (vals !! 0) (vals !! 1)
          send masterPid (Result nodeId node (serializeDoubleList result))
        "reluMatrix" -> do
          let vals = map (\x -> deserializeDoubleList (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = reluMatrix (vals !! 0)
          send masterPid (Result nodeId node (serializeDoubleList result))
        "upperHalf" -> do
          let vals = map (\x -> deserializeDoubleList (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = upperHalf (vals !! 0)
          send masterPid (Result nodeId node (serializeDoubleList result))
        "lowerHalf" -> do
          let vals = map (\x -> deserializeDoubleList (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = lowerHalf (vals !! 0)
          send masterPid (Result nodeId node (serializeDoubleList result))
        "leftHalf" -> do
          let vals = map (\x -> deserializeDoubleList (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = leftHalf (vals !! 0)
          send masterPid (Result nodeId node (serializeDoubleList result))
        "rightHalf" -> do
          let vals = map (\x -> deserializeDoubleList (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = rightHalf (vals !! 0)
          send masterPid (Result nodeId node (serializeDoubleList result))
        "transpose" -> do
          let vals = map (\x -> deserializeDoubleList (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = transpose (vals !! 0)
          send masterPid (Result nodeId node (serializeDoubleList result))
        "softmaxByRow" -> do
          let vals = map (\x -> deserializeDoubleList (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = softmaxByRow (vals !! 0)
          send masterPid (Result nodeId node (serializeDoubleList result))
        _ -> do
          error "Unknown function name"

    handleArgs :: Process ()
    handleArgs = do
      say $ "HANDLING node " ++ node
      send masterPid (Result nodeId node (serializeDouble (read node :: Double)))

remotable ['remoteCall]

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

dispatchJob :: ProcessId -> NodeId -> String -> HM.HashMap String String -> HM.HashMap String [String] -> Process ()
dispatchJob masterId nodeId node resultMap depGraph = do
  let remoteCallClosure = $(mkClosure 'remoteCall) (RemoteCall nodeId node masterId resultMap depGraph) -- MODIFIED
  _ <- spawn nodeId remoteCallClosure
  -- liftIO $ putStrLn $ "Sent task to worker: " ++ show nodeId ++ " for node: " ++ node
  return ()

unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM mb m = mb >>= flip unless m

allNodes :: HM.HashMap String [String] -> [String]
allNodes graph = HS.toList . HS.fromList $ HM.keys graph ++ concat (HM.elems graph)

sumVtmpValues :: HM.HashMap String String -> Double
sumVtmpValues = HM.foldrWithKey accumulateVtmpValues 0
  where
    accumulateVtmpValues key value acc =
      if "v_tmp" `isPrefixOf` key
        then acc + deserializeDouble value
        else acc

    isPrefixOf :: String -> String -> Bool
    isPrefixOf prefix str = take (length prefix) str == prefix

reverseDependencyGraph :: DependencyGraph -> HM.HashMap String [String]
reverseDependencyGraph = foldr reverseEdges HM.empty
  where
    reverseEdges (v, vs) revGraph = foldr (\v' acc -> HM.insertWith (++) v' [v] acc) revGraph vs

newTVarProcess :: a -> Process (TVar a)
newTVarProcess = liftIO . newTVarIO

readTVarProcess :: TVar a -> Process a
readTVarProcess mvar = liftIO $ readTVarIO mvar

modifyTVarProcess :: TVar a -> (a -> a) -> Process ()
modifyTVarProcess mvar func = liftIO . atomically $ modifyTVar' mvar func

getIndegreeCount :: HM.HashMap String [String] -> HM.HashMap String Int
getIndegreeCount graph = foldl' updateOutdegree initCounts $ HM.toList graph
  where
    initCounts = HM.map (const 0) graph
    updateOutdegree counts (node, edges) =
      let updatedHM = HM.insertWith (+) node (length edges) counts
       in foldl' (\acc edge -> HM.insertWith (const id) edge 0 acc) updatedHM edges

updateCounts :: [String] -> HM.HashMap String Int -> HM.HashMap String Int
updateCounts nodes hm = foldr (HM.adjust (subtract 1)) hm nodes

getIndegreeZeroNodes :: HM.HashMap String Int -> HM.HashMap String String -> [String] -> [String]
getIndegreeZeroNodes indegreeCount results queue =
  let cands = HM.keys $ HM.filter (== 0) indegreeCount
   in filter (\x -> not (x `HM.member` results || x `elem` queue)) cands

{-
Code for master. Master performs the following steps:
1, read input haskell program and build its dependency graph.
2, reverse every edge in the dependency graph to get the precendence graph.
3, find nodes with in-degree zero and add them to the queue.
4, run assignJobs and processReply. Both processes run continually and simultaneously.
   In particular, assignJobs will continually dispatch jobs from the queue to remote workers to be executed
   until all nodes in the dependency graph have been calculated; processReply will listen for response from
   remote workers and update result hashmap and decrement the in-degree of nodes pointed to by the executed
   task upon receiving worker response.
5, return the output of input program and time elapsed.
-}
runMaster :: Backend -> String -> [NodeId] -> Process ()
runMaster backend filepath workers = do
  start <- liftIO getCurrentTime
  say "Master started"
  if null workers
    then do
      say "No workers available, exiting"
      liftIO $ exitWith (ExitFailure 1)
    else say $ "Master discovered workers: " ++ show workers
  dependencyGraph <- liftIO $ buildGraph filepath >>= either (error . show) return
  let depGraph = HM.fromList dependencyGraph :: HM.HashMap String [String]
  let reversedDepGraph = reverseDependencyGraph dependencyGraph :: HM.HashMap String [String]
  let indegreeCount = getIndegreeCount depGraph :: HM.HashMap String Int
  let indegreeZeroNodes = HM.keys . HM.filter (== 0) $ indegreeCount :: [String]
  let nodeList = allNodes depGraph :: [String]

  queueVar <- newTVarProcess indegreeZeroNodes
  indegreeCountVar <- newTVarProcess indegreeCount
  resultsVar <- newTVarProcess HM.empty
  idleWorkersVar <- newTVarProcess workers

  say $ "Initial queue: " ++ show indegreeZeroNodes

  master <- getSelfPid

  let assignJobs :: Process ()
      assignJobs = do
        queue <- readTVarProcess queueVar
        results <- readTVarProcess resultsVar
        idleWorkers <- readTVarProcess idleWorkersVar
        if (null queue && length results == length nodeList)
          then do
            say $ "Returning from assignJobs"
            return ()
          else
            if (not (null idleWorkers) && not (null queue))
              then do
                let worker = head idleWorkers
                let job = head queue
                modifyTVarProcess idleWorkersVar tail
                modifyTVarProcess queueVar tail
                dispatchJob master worker job results depGraph
                liftIO $ threadDelay 1000
                assignJobs
              else do
                liftIO $ threadDelay 1000
                assignJobs

  let processReplies :: Process ()
      processReplies = do
        results <- readTVarProcess resultsVar
        queue <- readTVarProcess queueVar
        if length results == length nodeList
          then do
            say $ "Final result: " ++ show (sumVtmpValues results)
            say $ "Returning from processReplies"
            return ()
          else do
            Result worker node result <- expect
            -- say $ "Received result: " ++ show result ++ " from worker: " ++ show worker ++ " for node: " ++ node
            -- say $ "PROGRESS" ++ show (length results) ++ " " ++ show (length nodeList)
            modifyTVarProcess idleWorkersVar (++ [worker])
            modifyTVarProcess resultsVar (HM.insert node result)
            modifyTVarProcess indegreeCountVar (updateCounts (fromMaybe [] $ HM.lookup node reversedDepGraph))
            updatedIndegrees <- readTVarProcess indegreeCountVar
            updatedResults <- readTVarProcess resultsVar
            -- say $ "RESULTS" ++ show updatedResults ++ " " ++ show nodeList
            modifyTVarProcess queueVar (++ getIndegreeZeroNodes updatedIndegrees updatedResults queue)
            processReplies

  -- task <- async (AsyncTask assignJobs)
  -- processReplies
  pid1 <- spawnLocal assignJobs
  processReplies
  end <- liftIO getCurrentTime
  say $ "Total time: " ++ show (diffUTCTime end start)
  -- cancel task
  terminateAllSlaves backend

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["master", host, port, filepath] -> do
      backend <- initializeBackend host port myRemoteTable
      startMaster backend (runMaster backend filepath)
    ["slave", host, port] -> do
      backend <- initializeBackend host port myRemoteTable
      startSlave backend
    _ -> putStrLn "Invalid command. Use 'master <host> <port> <filepath>' or 'slave <host> <port>'."