{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module RemoteCall where

import Control.Distributed.Process
import Control.Monad (forM_, replicateM)
import Data.Binary
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.Typeable
import GHC.Generics

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

data RemoteCall = RemoteCall NodeId String ProcessId (HM.HashMap String String) (HM.HashMap String [String]) -- MODIFIED
  deriving (Show, Typeable, Generic)

instance Binary RemoteCall
