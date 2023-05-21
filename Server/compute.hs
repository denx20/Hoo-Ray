{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Monad (forM, forM_)

import DistribUtils
import SerializedFunction

import Text.Printf
import Data.Binary
import Data.Typeable
import Data.Maybe
import GHC.Generics (Generic)

-- We also need to send the type of the function to the remote server
-- For this iteration, let's just do functions with integer-valued results

-- To-do: handle functions with an arbitrary number of arguments

data Message =
  -- Ping ProcessId | Pong ProcessId
  Command SerializedFunction ProcessId | Result Integer ProcessId
  deriving (Typeable, Generic)

instance Binary Message

add :: Int -> Int -> Int
add x y = x + y

worker :: Process ()
worker = do
  Command serializedF from <- expect
  deserializedF <- fromMaybe Nothing (deserializeFunction serializedF)
  say $ printf "received %s from %s" (show serializedF) (show from)
  mypid <- getSelfPid
  send from (Result 0 mypid) -- Change this to result of actual computation

remotable ['worker]

master :: Process ()
master = do
  node <- getSelfNode

  say $ printf "spawning 10 workers on %s" (show node)
  mypid <- getSelfPid
  workerPids <- forM ([1..10] :: [Int]) $ \_ -> spawn node $(mkStaticClosure 'worker)

  -- say $ printf "worker pids: %s" (show workerPids)

  say $ printf "sending pings to workers"
  forM_ workerPids $ \pid -> send pid (Command (serializeFunction "add" (add:: Int -> Int -> Int)) mypid)

  forM_ workerPids $ \_ -> do
    Result _ _ <- expect
    say "Received result"

  terminate

main :: IO ()
main = distribMain (\_ -> master) Main.__remoteTable

