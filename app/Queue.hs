{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Control.Concurrent.MVar
  ( MVar,
    modifyMVar_,
    newMVar,
    readMVar,
  )
import Control.Distributed.Process
  ( NodeId,
    Process,
    ProcessId,
    RemoteTable,
    expectTimeout,
    getSelfPid,
    liftIO,
    say,
    send,
    spawn,
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
import Control.Monad (when)
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
    deserializeDoubleList,
    extractMiddle,
    generateRandomMatrix,
    leftHalf,
    lowerHalf,
    madd,
    mmult,
    reluMatrix,
    rightHalf,
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

data Message = Result NodeId String String
  deriving (Typeable, Generic)

instance Binary Message

data RemoteCall = RemoteCall NodeId String ProcessId (HM.HashMap String String) (HM.HashMap String [String])
  deriving (Show, Typeable, Generic)

instance Binary RemoteCall

class Serializable a where
  serialize :: a -> String
  deserialize :: String -> a

instance Serializable Int where
  serialize = show
  deserialize s = round (read s :: Double)

instance Serializable Double where
  serialize = show
  deserialize = read

instance Serializable [[Double]] where
  serialize = serializeDoubleList
  deserialize = deserializeDoubleList

class ProcessStep a where
  action :: a -> [String] -> IO String

data CalculateMatrix = CalculateMatrix

instance ProcessStep CalculateMatrix where
  action CalculateMatrix args =
    let [d1, d2, d3, d4, d5, d6] = map deserialize args :: [Double]
     in return $ show $ calculateMatrix d1 d2 d3 d4 d5 d6

data GenerateRandomMatrix = GenerateRandomMatrix

instance ProcessStep GenerateRandomMatrix where
  action GenerateRandomMatrix args =
    let [d1, d2, d3, d4] = map deserialize args :: [Double]
     in return $ serialize $ generateRandomMatrix (round d1) (round d2) d3 (round d4)

data Mmult = Mmult

instance ProcessStep Mmult where
  action Mmult args =
    let [d1, d2] = map deserialize args :: [[[Double]]]
     in return $ serialize $ mmult d1 d2

data SumMatrix = SumMatrix

instance ProcessStep SumMatrix where
  action SumMatrix args =
    let [d1] = map deserialize args :: [[[Double]]]
     in return $ serialize $ sumMatrix d1

data Madd = Madd

instance ProcessStep Madd where
  action Madd args =
    let [d1, d2] = map deserialize args :: [[[Double]]]
     in return $ serialize $ madd d1 d2

data ReluMatrix = ReluMatrix

instance ProcessStep ReluMatrix where
  action ReluMatrix args =
    let [d1] = map deserialize args :: [[[Double]]]
     in return $ serialize $ reluMatrix d1

data UpperHalf = UpperHalf

instance ProcessStep UpperHalf where
  action UpperHalf args =
    let [d1] = map deserialize args :: [[[Double]]]
     in return $ serialize $ upperHalf d1

data LowerHalf = LowerHalf

instance ProcessStep LowerHalf where
  action LowerHalf args =
    let [d1] = map deserialize args :: [[[Double]]]
     in return $ serialize $ lowerHalf d1

data LeftHalf = LeftHalf

instance ProcessStep LeftHalf where
  action LeftHalf args =
    let [d1] = map deserialize args :: [[[Double]]]
     in return $ serialize $ leftHalf d1

data RightHalf = RightHalf

instance ProcessStep RightHalf where
  action RightHalf args =
    let [d1] = map deserialize args :: [[[Double]]]
     in return $ serialize $ rightHalf d1

data Transpose = Transpose

instance ProcessStep Transpose where
  action Transpose args =
    let [d1] = map deserialize args :: [[[Double]]]
     in return $ serialize $ transpose d1

data SoftmaxByRow = SoftmaxByRow

instance ProcessStep SoftmaxByRow where
  action SoftmaxByRow args =
    let [d1] = map deserialize args :: [[[Double]]]
     in return $ serialize $ softmaxByRow d1

data SomeProcessStep = forall a. (ProcessStep a) => SomeProcessStep a

processSteps :: HM.HashMap String SomeProcessStep
processSteps =
  HM.fromList
    [ ("calculateMatrix", SomeProcessStep CalculateMatrix),
      ("generateRandomMatrix", SomeProcessStep GenerateRandomMatrix),
      ("mmult", SomeProcessStep Mmult),
      ("sumMatrix", SomeProcessStep SumMatrix),
      ("madd", SomeProcessStep Madd),
      ("reluMatrix", SomeProcessStep ReluMatrix),
      ("upperHalf", SomeProcessStep UpperHalf),
      ("lowerHalf", SomeProcessStep LowerHalf),
      ("leftHalf", SomeProcessStep LeftHalf),
      ("rightHalf", SomeProcessStep RightHalf),
      ("transpose", SomeProcessStep Transpose),
      ("softmaxByRow", SomeProcessStep SoftmaxByRow)
    ]

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
      send masterPid (Result nodeId node result)

    {-
    Handle function execution. Define the task for the remote worker to perform for each type of operation.
    Currently supports the following operations: "calculateMatrix", "generateRandomMatrix", "mmult", and "sumMatrix"
    -}
    handleF :: Process ()
    handleF = do
      say $ "HANDLING function node " ++ node
      let deps = fromMaybe [] (HM.lookup node depGraph)
      let vals = map (\x -> fromMaybe "" (HM.lookup x resultMap)) deps
      case HM.lookup (extractMiddle node) processSteps of
        Just (SomeProcessStep step) -> do
          result <- liftIO $ action step vals
          send masterPid (Result nodeId node result)
        Nothing -> error "Unknown function name"

    handleArgs :: Process ()
    handleArgs = do
      -- say $ "HANDLING node " ++ node
      send masterPid (Result nodeId node (serialize (read node :: Double)))

remotable ['remoteCall]

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

dispatchJob :: ProcessId -> NodeId -> String -> HM.HashMap String String -> HM.HashMap String [String] -> Process ()
dispatchJob masterId nodeId node resultMap depGraph = do
  let remoteCallClosure = $(mkClosure 'remoteCall) (RemoteCall nodeId node masterId resultMap depGraph)
  _ <- spawn nodeId remoteCallClosure
  -- liftIO $ putStrLn $ "Sent task to worker: " ++ show nodeId ++ " for node: " ++ node
  return ()

allNodes :: HM.HashMap String [String] -> [String]
allNodes graph = HS.toList . HS.fromList $ HM.keys graph ++ concat (HM.elems graph)

reverseDependencyGraph :: DependencyGraph -> HM.HashMap String [String]
reverseDependencyGraph = foldr reverseEdges HM.empty
  where
    reverseEdges (v, vs) revGraph = foldr (\v' acc -> HM.insertWith (++) v' [v] acc) revGraph vs

newMVarProcess :: a -> Process (MVar a)
newMVarProcess = liftIO . newMVar

readMVarProcess :: MVar a -> Process a
readMVarProcess mvar = liftIO $ readMVar mvar

modifyMVarProcess :: MVar a -> (a -> a) -> Process ()
modifyMVarProcess mvar func = liftIO $ modifyMVar_ mvar (return . func)

getIndegreeCount :: HM.HashMap String [String] -> HM.HashMap String Int
getIndegreeCount graph = foldl' updateOutdegree initCounts $ HM.toList graph
  where
    initCounts = HM.map (const 0) graph
    updateOutdegree counts (node, edges) =
      let updatedHM = HM.insertWith (+) node (length edges) counts
       in foldl' (\acc edge -> HM.insertWith (const id) edge 0 acc) updatedHM edges

updateCounts :: [String] -> HM.HashMap String Int -> HM.HashMap String Int
updateCounts nodes hm = foldr (HM.adjust (subtract 1)) hm nodes

getIndegreeZeroNodes :: HM.HashMap String Int -> HS.HashSet String -> [String] -> [String]
getIndegreeZeroNodes indegreeCount pendingNodes queue =
  let cands = HM.keys $ HM.filter (== 0) indegreeCount
   in filter (\x -> notElem x queue && HS.member x pendingNodes) cands

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
  when (null workers) $ do
    say "No workers available, exiting"
    liftIO $ exitWith (ExitFailure 1)
  dependencyGraph <- liftIO $ buildGraph filepath >>= either (error . show) return
  let depGraph = HM.fromList dependencyGraph :: HM.HashMap String [String]
  let reversedDepGraph = reverseDependencyGraph dependencyGraph :: HM.HashMap String [String]
  let indegreeCount = getIndegreeCount depGraph :: HM.HashMap String Int
  let indegreeZeroNodes = HM.keys . HM.filter (== 0) $ indegreeCount :: [String]
  let nodeList = allNodes depGraph :: [String]

  queueVar <- newMVarProcess indegreeZeroNodes
  indegreeCounMVar <- newMVarProcess indegreeCount
  resultsVar <- newMVarProcess HM.empty
  idleWorkersVar <- newMVarProcess workers
  pendingNodesVar <- newMVarProcess $ HS.fromList nodeList
  finalResultVar <- newMVarProcess Nothing

  master <- getSelfPid
  let process :: Process ()
      process = do
        queue <- readMVarProcess queueVar
        results <- readMVarProcess resultsVar
        idleWorkers <- readMVarProcess idleWorkersVar
        maybeResult <- expectTimeout 0
        case maybeResult of
          Just (Result worker node result) -> do
            modifyMVarProcess idleWorkersVar (++ [worker])
            modifyMVarProcess resultsVar (HM.insert node result)
            modifyMVarProcess indegreeCounMVar (updateCounts (fromMaybe [] $ HM.lookup node reversedDepGraph))
            updatedIndegrees <- readMVarProcess indegreeCounMVar
            pendingNodes <- readMVarProcess pendingNodesVar
            modifyMVarProcess queueVar (++ getIndegreeZeroNodes updatedIndegrees pendingNodes queue)
            updatedResults <- readMVarProcess resultsVar
            when (length updatedResults == length nodeList) $ do
              modifyMVarProcess finalResultVar (const (Just result))
              return ()
          Nothing -> return ()
        when (not (null idleWorkers) && not (null queue)) $ do
          let worker = head idleWorkers
          let job = head queue
          modifyMVarProcess idleWorkersVar tail
          modifyMVarProcess queueVar tail
          modifyMVarProcess pendingNodesVar (HS.delete job)
          dispatchJob master worker job results depGraph
        finalResult <- readMVarProcess finalResultVar
        case finalResult of
          Just result -> do
            say $ "Final result: " ++ result
            return ()
          Nothing -> process
  process
  end <- liftIO getCurrentTime
  say $ "Total time: " ++ show (diffUTCTime end start)
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
