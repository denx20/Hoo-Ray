-- Example code taken from https://hackage.haskell.org/package/distributed-process-simplelocalnet-0.2.4/docs/Control-Distributed-Process-Backend-SimpleLocalnet.html
-- Should be able to be run with
-- cabal run distributed-example slave localhost 8081 &
-- cabal run distributed-example slave localhost 8082 &
-- cabal run distributed-example slave localhost 8083 &
-- cabal run distributed-example master localhost 8084
--
-- Figured this out with Joe

import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet

master :: Backend -> [NodeId] -> Process ()
master backend slaves = do
  -- Do something interesting with the slaves
  liftIO . putStrLn $ "Slaves: " ++ show slaves
  -- Terminate the slaves when the master terminates (this is optional)
  terminateAllSlaves backend

main :: IO ()
main = do
  args <- getArgs

  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port initRemoteTable
      startMaster backend (master backend)
    ["slave", host, port] -> do
      backend <- initializeBackend host port initRemoteTable
      startSlave backend
    _ -> return ()