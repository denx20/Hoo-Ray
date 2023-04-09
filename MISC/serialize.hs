import SerializedFunction

-- Example operations
squareThenAddTwo :: Int -> Int
squareThenAddTwo x = x^2 + 2

sumOfSquares :: Int -> Int -> Int
sumOfSquares x y = x^2 + y^2

add :: Int -> Int -> Int
add x y = x + y

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