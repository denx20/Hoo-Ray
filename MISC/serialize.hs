{-# LANGUAGE DeriveDataTypeable #-}

import Data.Typeable
import Data.Dynamic

data SerializedFunction = SerializedFunction String Dynamic
    deriving (Typeable, Show)

serializeFunction :: (Typeable a, Typeable b) => String -> (a -> b) -> SerializedFunction
serializeFunction name f = SerializedFunction name (toDyn f)

deserializeFunction :: (Typeable a, Typeable b) => SerializedFunction -> Maybe (a -> b)
deserializeFunction (SerializedFunction _ dynFunc) = fromDynamic dynFunc

main :: IO ()
main = do
    let serializedAdd = serializeFunction "add" (add :: Int -> Int -> Int)
    putStrLn $ "Serialized function: " ++ show serializedAdd

    let deserializedAdd = deserializeFunction serializedAdd :: Maybe (Int -> Int -> Int)

    case deserializedAdd of
        Just addFunc -> putStrLn $ "Deserialized function result (3 + 5): " ++ show (addFunc 3 5)
        Nothing -> putStrLn "Failed to deserialize the function"

add :: Int -> Int -> Int
add x y = x + y