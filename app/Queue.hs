{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Concurrent.MVar
  ( modifyMVar,
    modifyMVar_,
    newMVar,
    readMVar,
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
  )
import Control.Distributed.Process.Async
  ( AsyncTask (AsyncTask),
    async,
  )
import Control.Distributed.Process.Backend.SimpleLocalnet
  ( Backend,
    initializeBackend,
    startMaster,
    startSlave,
    terminateAllSlaves,
  )
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Monad (forM_, replicateM)
import Data.Binary (Binary (get, put))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.List (foldl', nub, transpose)
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

instance (Binary k, Binary v, Eq k, Hashable k) => Binary (HM.HashMap k v) where
  put hashMap = do
    put (HM.size hashMap)
    forM_ (HM.toList hashMap) $ \(k, v) -> do
      put k
      put v

  get = do
    size <- get
    fmap HM.fromList $
      replicateM size $ do
        k <- get
        v <- get
        return (k, v)

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
      say $ "FINISH node " ++ node

    {-
    Handle function execution. Define the task for the remote worker to perform for each type of operation.
    Currently supports the following operations: "calculateMatrix", "generateRandomMatrix", "mmult", and "sumMatrix"
    -}
    handleF :: Process ()
    handleF = do
      say $ "HANDLING node " ++ node
      let deps = fromMaybe [] (HM.lookup node depGraph)
      say $ "Node function name" ++ extractMiddle node
      case extractMiddle node of
        "calculateMatrix" -> do
          let vals = map (\x -> deserializeDouble (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = calculateMatrix (vals !! 0) (vals !! 1) (vals !! 2) (vals !! 3) (vals !! 4) (vals !! 5)
          send masterPid (Result nodeId node (serializeDouble result))
          say $ "FINISH node " ++ node
        "generateRandomMatrix" -> do
          let vals = map (\x -> deserializeDouble (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = generateRandomMatrix (round (vals !! 0)) (round (vals !! 1)) (vals !! 2) (round (vals !! 3))
          send masterPid (Result nodeId node (serializeDoubleList result))
          say $ "FINISH node " ++ node
        "mmult" -> do
          let vals = map (\x -> deserializeDoubleList (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = mmult (vals !! 0) (vals !! 1)
          send masterPid (Result nodeId node (serializeDoubleList result))
          say $ "FINISH node " ++ node
        "sumMatrix" -> do
          let vals = map (\x -> deserializeDoubleList (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = sumMatrix (vals !! 0)
          send masterPid (Result nodeId node (serializeDouble result))
          say $ "FINISH node " ++ node
        "madd" -> do
          let vals = map (\x -> deserializeDoubleList (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = madd (vals !! 0) (vals !! 1)
          send masterPid (Result nodeId node (serializeDoubleList result))
          say $ "FINISH node " ++ node
        "reluMatrix" -> do
          let vals = map (\x -> deserializeDoubleList (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = reluMatrix (vals !! 0)
          send masterPid (Result nodeId node (serializeDoubleList result))
          say $ "FINISH node " ++ node
        "upperHalf" -> do
          let vals = map (\x -> deserializeDoubleList (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = upperHalf (vals !! 0)
          send masterPid (Result nodeId node (serializeDoubleList result))
          say $ "FINISH node " ++ node
        "lowerHalf" -> do
          let vals = map (\x -> deserializeDoubleList (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = lowerHalf (vals !! 0)
          send masterPid (Result nodeId node (serializeDoubleList result))
          say $ "FINISH node " ++ node
        "leftHalf" -> do
          let vals = map (\x -> deserializeDoubleList (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = leftHalf (vals !! 0)
          send masterPid (Result nodeId node (serializeDoubleList result))
          say $ "FINISH node " ++ node
        "rightHalf" -> do
          let vals = map (\x -> deserializeDoubleList (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = rightHalf (vals !! 0)
          send masterPid (Result nodeId node (serializeDoubleList result))
          say $ "FINISH node " ++ node
        "transpose" -> do
          let vals = map (\x -> deserializeDoubleList (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = transpose (vals !! 0)
          send masterPid (Result nodeId node (serializeDoubleList result))
          say $ "FINISH node " ++ node
        "softmaxByRow" -> do
          let vals = map (\x -> deserializeDoubleList (fromMaybe "" (HM.lookup x resultMap))) deps
          let result = softmaxByRow (vals !! 0)
          send masterPid (Result nodeId node (serializeDoubleList result))
          say $ "FINISH node " ++ node
        _ -> do
          say $ "SUS function name"

    handleArgs :: Process ()
    handleArgs = do
      -- say $ "HANDLING node " ++ node
      send masterPid (Result nodeId node (serializeDouble (read node :: Double)))

remotable ['remoteCall]

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

dispatchJob :: ProcessId -> NodeId -> String -> HM.HashMap String String -> HM.HashMap String [String] -> Process ()
dispatchJob masterId nodeId node resultMap depGraph = do
  let remoteCallClosure = $(mkClosure 'remoteCall) (RemoteCall nodeId node masterId resultMap depGraph) -- MODIFIED
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
sumVtmpValues = HM.foldrWithKey accumulateVtmpValues 0
  where
    accumulateVtmpValues key value acc =
      if "v_tmp" `isPrefixOf` key
        then acc + deserializeDouble value
        else acc

    isPrefixOf :: String -> String -> Bool
    isPrefixOf prefix str = take (length prefix) str == prefix

updateIndegreeMap :: String -> HM.HashMap String Int -> HM.HashMap String Int
updateIndegreeMap =
  HM.adjust (\indegree -> indegree - 1)

reverseDependencyGraph :: DependencyGraph -> HM.HashMap String [String]
reverseDependencyGraph = foldr reverseEdges HM.empty
  where
    reverseEdges (v, vs) revGraph = foldr (\v' acc -> HM.insertWith (++) v' [v] acc) revGraph vs

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
master :: Backend -> String -> [NodeId] -> Process ()
master backend filepath workers = do
  start <- liftIO getCurrentTime
  say "Master started"
  if null workers
    then do
      say "No workers available, exiting"
      liftIO $ exitWith (ExitFailure 1)
    else say $ "Master discovered workers: " ++ show workers
  dependencyGraph <- liftIO $ buildGraph filepath >>= either (error . show) return
  let depGraph = HM.fromList dependencyGraph :: HM.HashMap String [String]
  let reversedGraph = reverseDependencyGraph dependencyGraph :: HM.HashMap String [String]
  let indegreeCount = indegreeCounts dependencyGraph :: [(String, Int)]
  let indegreeZeroNodes = map fst $ filter (\(_, indegree) -> indegree == 0) indegreeCount :: [String]
  let nodeList = nub $ concatMap (uncurry (:)) dependencyGraph :: [String]

  workerListVar <- liftIO $ newMVar workers
  queueVar <- liftIO $ newMVar indegreeZeroNodes
  indegreeMapVar <- liftIO $ newMVar $ HM.fromList indegreeCount
  resultsVar <- liftIO $ newMVar HM.empty
  visitVar <- liftIO $ newMVar $ HS.fromList nodeList
  idleWorkerVar <- liftIO $ newMVar $ HS.fromList workers

  masterPid <- getSelfPid

  let assignJobs :: Process ()
      assignJobs = do
        queue <- liftIO $ readMVar queueVar :: Process [String]
        results <- liftIO $ readMVar resultsVar
        idle_workers <- liftIO $ readMVar idleWorkerVar
        if null queue && length results == length nodeList
          then do
            say $ "Final result: " ++ show (sumVtmpValues results)
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
            say $ "Current queue: " ++ show queue
            assignJobs

  let processReply :: Process ()
      processReply = do
        results <- liftIO $ readMVar resultsVar
        idle_workers <- liftIO $ readMVar idleWorkerVar
        if length results == length nodeList
          then do
            -- get the value for key "v_predsum" from results hashmap
            -- sum all the values with key "v_tmp" from results hashmap
            say $ "Final result: " ++ show (sumVtmpValues results)
            return ()
          else do
            if length results > 45
              then do
                say $ "Final result: " ++ (results HM.! "v_predsum")
                Result worker node result <- expect
                idle_workers' <- liftIO $ modifyMVar_ idleWorkerVar $ \_ -> return $ HS.insert worker idle_workers
                visit <- liftIO $ readMVar visitVar
                -- say $ "Visit set: " ++ (show visit)
                visit' <- liftIO $ modifyMVar visitVar $ \_ -> return (HS.delete node visit, HS.delete node visit)
                -- liftIO $ putStrLn $ "Received result: " ++ node ++ " -> " ++ (show result)
                results <- liftIO $ readMVar resultsVar
                results' <- liftIO $ modifyMVar resultsVar $ \_ -> return (HM.insert node result results, HM.insert node result results)

                say $ "Result map: " ++ show (length results')
                say $ "Node list: " ++ show (length nodeList)
                indegrees <- liftIO $ readMVar indegreeMapVar
                let updatedIndegrees = foldr updateIndegreeMap indegrees (fromMaybe [] $ HM.lookup node reversedGraph)
                liftIO $ modifyMVar_ indegreeMapVar $ \_ -> return updatedIndegrees
                liftIO $
                  modifyMVar_ queueVar $ \queue ->
                    let newNodes = [v | (v, indegree) <- HM.toList updatedIndegrees, indegree == 0, v `notElem` queue, v `elem` visit']
                     in do
                          return $ queue ++ newNodes
                processReply
              else do
                Result worker node result <- expect
                idle_workers' <- liftIO $ modifyMVar_ idleWorkerVar $ \_ -> return $ HS.insert worker idle_workers
                visit <- liftIO $ readMVar visitVar
                -- say $ "Visit set: " ++ (show visit)
                visit' <- liftIO $ modifyMVar visitVar $ \_ -> return (HS.delete node visit, HS.delete node visit)
                -- liftIO $ putStrLn $ "Received result: " ++ node ++ " -> " ++ (show result)
                results <- liftIO $ readMVar resultsVar
                results' <- liftIO $ modifyMVar resultsVar $ \_ -> return (HM.insert node result results, HM.insert node result results)

                say $ "Result map: " ++ show (length results')
                say $ "Node list: " ++ show (length nodeList)
                indegrees <- liftIO $ readMVar indegreeMapVar
                let updatedIndegrees = foldr updateIndegreeMap indegrees (fromMaybe [] $ HM.lookup node reversedGraph)
                liftIO $ modifyMVar_ indegreeMapVar $ \_ -> return updatedIndegrees
                liftIO $
                  modifyMVar_ queueVar $ \queue ->
                    let newNodes = [v | (v, indegree) <- HM.toList updatedIndegrees, indegree == 0, v `notElem` queue, v `elem` visit']
                     in do
                          return $ queue ++ newNodes
                processReply

  _ <- async (AsyncTask assignJobs)
  processReply
  terminateAllSlaves backend
  end <- liftIO getCurrentTime
  say $ "Total time: " ++ show (diffUTCTime end start)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["master", host, port, filepath] -> do
      backend <- initializeBackend host port myRemoteTable
      startMaster backend (master backend filepath)
    ["slave", host, port] -> do
      backend <- initializeBackend host port myRemoteTable
      startSlave backend
    _ -> putStrLn "Invalid command. Use 'master <host> <port> <filepath>' or 'slave <host> <port>'."
