{-# LANGUAGE DeriveDataTypeable #-}

import Data.Typeable
import Data.Dynamic

data SerializedFunction = SerializedFunction String Dynamic
    deriving (Typeable, Show)

serializeFunction :: (Typeable a, Typeable b) => String -> (a -> b) -> SerializedFunction
serializeFunction name f = SerializedFunction name (toDyn f)

deserializeFunction :: (Typeable a, Typeable b) => SerializedFunction -> Maybe (a -> b)
deserializeFunction (SerializedFunction _ dynFunc) = fromDynamic dynFunc

-- Squares the input then add two. This can be an arbitrarily complex function
squareThenAddTwo :: Int -> Int
squareThenAddTwo x = x^2 + 2

sumOfSquares :: Int -> Int -> Int
sumOfSquares x y = x^2 + y^2

main :: IO ()
main = do
    let serializedAdd = serializeFunction "add" (add :: Int -> Int -> Int)
    putStrLn $ "Serialized function: " ++ show serializedAdd

    let deserializedAdd = deserializeFunction serializedAdd :: Maybe (Int -> Int -> Int)

    case deserializedAdd of
        Just addFunc -> putStrLn $ "Deserialized function result (3 + 5): " ++ show (addFunc 3 5)
        Nothing -> putStrLn "Failed to deserialize the function"

    let serializedF = serializeFunction "squareThenAddTwo" (squareThenAddTwo :: Int -> Int)
    let deserializedF = deserializeFunction serializedF :: Maybe (Int -> Int)

    case deserializedF of
        Just f -> putStrLn $ "Deserialized squareThenAddTwo result: " ++ show (f 5)
        Nothing -> putStrLn "Failed to deserialize the function"

    let serializedF = serializeFunction "sumOfSquares" (sumOfSquares :: Int -> Int -> Int)
    let deserializedF = deserializeFunction serializedF :: Maybe (Int -> Int -> Int)

    case deserializedF of
        Just f -> putStrLn $ "Deserialized sumOfSquares result: " ++ show (f 2 3)
        Nothing -> putStrLn "Failed to deserialize the function"

add :: Int -> Int -> Int
add x y = x + y