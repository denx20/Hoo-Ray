-- Example code taken from https://hackage.haskell.org/package/distributed-process-simplelocalnet-0.2.4/docs/Control-Distributed-Process-Backend-SimpleLocalnet.html
-- Should be able to be run with
-- cabal run distributed-example slave localhost 8081 &
-- cabal run distributed-example slave localhost 8082 &
-- cabal run distributed-example slave localhost 8083 &
-- cabal run distributed-example master localhost 8084
-- But currently does not work on Duke's network as UDP was blocked. Holding off until Joe responds back to me.

--
-- (base) ➜  Downloads ./iperf3 -c 152.3.52.171 -u
-- Connecting to host 152.3.52.171, port 5201
-- [  4] local 10.197.166.144 port 53442 connected to 152.3.52.171 port 5201
-- [ ID] Interval           Transfer     Bandwidth       Total Datagrams
-- [  4]   0.00-1.00   sec   128 KBytes  1.05 Mbits/sec  16
-- [  4]   1.00-2.00   sec   128 KBytes  1.05 Mbits/sec  16
-- [  4]   2.00-3.00   sec   128 KBytes  1.05 Mbits/sec  16
-- [  4]   3.00-4.00   sec   128 KBytes  1.05 Mbits/sec  16
-- [  4]   4.00-5.00   sec   128 KBytes  1.05 Mbits/sec  16
-- [  4]   5.00-6.00   sec   128 KBytes  1.05 Mbits/sec  16
-- [  4]   6.00-7.01   sec   128 KBytes  1.04 Mbits/sec  16
-- [  4]   7.01-8.00   sec   128 KBytes  1.05 Mbits/sec  16
-- [  4]   8.00-9.00   sec   128 KBytes  1.05 Mbits/sec  16
-- [  4]   9.00-10.00  sec   128 KBytes  1.05 Mbits/sec  16
-- - - - - - - - - - - - - - - - - - - - - - - - - -
-- [ ID] Interval           Transfer     Bandwidth       Jitter    Lost/Total Datagrams
-- [  4]   0.00-10.00  sec  1.25 MBytes  1.05 Mbits/sec  3912702.089 ms  0/159 (0%)
-- [  4] Sent 159 datagrams

-- iperf Done.
---

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