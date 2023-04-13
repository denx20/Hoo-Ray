{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}


import Control.Concurrent.Async (Async, async, wait)
import Control.Distributed.Process (Process, Closure, call, RemoteTable, liftIO)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process.Node (initRemoteTable, runProcess, newLocalNode)
import Control.Distributed.Process.Backend.SimpleLocalnet (initializeBackend)
import Control.Monad (void, forM_)
import Data.Typeable (Typeable)
import Data.List ((\\), nub)
import Data.Binary 
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.MVar

-- Add necessary imports
import Control.Distributed.Process.Serializable (Serializable)
import GHC.Generics (Generic)

type DependencyGraph = [(String, [String])]

data RemoteCall = RemoteCall (String, [String])
  deriving (Show, Typeable, Generic)

instance Binary RemoteCall

remoteCall :: RemoteCall -> Process Float
remoteCall (RemoteCall _) = return 1.0 -- TODO: This is dummy, need to change. RemoteCall should return remote worker's execution output


reverseDependencyGraph :: DependencyGraph -> DependencyGraph  
reverseDependencyGraph g = concatMap reverseEdges g
  where
    reverseEdges (v, vs) = [(v', [v]) | v' <- vs]

findIndegreeZeroNodes :: DependencyGraph -> [String]
findIndegreeZeroNodes g =
  let allNodes = nub [v | (v, _) <- g]
      nodesWithEdges = nub $ concatMap snd g
   in allNodes \\ nodesWithEdges


remotable ['remoteCall]

-- TODO: executeFunctionAsync is still buggy
executeFunctionAsync :: Int -> MVar (HM.HashMap String Int) -> (String, [String]) -> Process (Async Int)
executeFunctionAsync workerId resultsVar edge = do
  let closure = mkClosure 'remoteCall (RemoteCall edge)
  async $ do
    result <- call closure
    liftIO $ modifyMVar_ resultsVar $ \hm -> return $ HM.insert (fst edge) result hm
    return result

-- TODO: executeFunctions this is still buggy
executeFunctions :: RemoteTable -> [String] -> DependencyGraph -> IO (HM.HashMap String Int)
executeFunctions rtable indegreeZeroNodes depGraph = do
  resultsVar <- newMVar HM.empty
  workerPool <- newMVar [1..10] -- We have 10 remote workers
  let master = do
        forM_ indegreeZeroNodes $ \node -> do
          let dependencyEdges = [(v, deps) | (v, deps) <- depGraph, node `elem` deps]
          forM_ dependencyEdges $ \edge -> do
            workerId <- liftIO $ takeMVar workerPool
            asyncHandle <- executeFunctionAsync workerId resultsVar edge
            result <- wait asyncHandle
            let dependent = fst edge
            liftIO $ modifyMVar_ resultsVar $ \hm -> return $ HM.insert dependent result hm
            liftIO $ putStrLn $ "Received result for " ++ dependent ++ ": " ++ show result
            liftIO $ putMVar workerPool workerId
  runProcessWithRTS rtable master
  readMVar resultsVar


main :: IO ()
main = do
  let dependencyGraph = [("A", ["B", "C"]), ("B", ["D"]), ("C", ["D"]), ("D", [])]
  let reversedGraph = reverseDependencyGraph dependencyGraph
  let indegreeZeroNodes = findIndegreeZeroNodes reversedGraph
  putStrLn $ "Reversed Graph: " ++ show reversedGraph
  putStrLn $ "Nodes with in-degree 0: " ++ show indegreeZeroNodes

  backend <- initializeBackend "localhost" "8080" rtable
  node <- newLocalNode backend
  resultMap <- runProcess node $ executeFunctions (__remoteTable initRemoteTable) indegreeZeroNodes reversedGraph
  putStrLn $ "Execution results: " ++ show resultMap

  where
    rtable :: RemoteTable
    rtable = __remoteTable initRemoteTable

