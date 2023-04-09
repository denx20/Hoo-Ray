{-# LANGUAGE DeriveDataTypeable #-}

module SerializedFunction
    ( SerializedFunction
    , serializeFunction
    , deserializeFunction
    ) where

import Data.Typeable
import Data.Dynamic

data SerializedFunction = SerializedFunction String Dynamic
    deriving (Typeable, Show)

serializeFunction :: (Typeable a, Typeable b) => String -> (a -> b) -> SerializedFunction
serializeFunction name f = SerializedFunction name (toDyn f)

deserializeFunction :: (Typeable a, Typeable b) => SerializedFunction -> Maybe (a -> b)
deserializeFunction (SerializedFunction _ dynFunc) = fromDynamic dynFunc
