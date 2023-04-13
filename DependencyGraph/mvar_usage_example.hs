import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Monad (forM_, replicateM_)
import qualified Data.HashMap.Strict as HM
import System.Environment (getArgs)
import Data.List ((\\), nub)

import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable
import Text.Printf

-- For using the node list FIFO
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

-- Rotating a list and printing the first element
main :: IO ()
main = do
    let list = [1, 2, 3]
    listVar <- newMVar list
    replicateM_ 5 $ do
        currentList <- takeMVar listVar
        let rotatedList = rotate 1 currentList
        print (head rotatedList)
        putMVar listVar rotatedList