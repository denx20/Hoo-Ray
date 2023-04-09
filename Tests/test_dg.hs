{-# INCLUDE "../DependencyGraph/graph.hs" #-}

import Test.HUnit
import qualified Graph

testAddition = TestCase (assertEqual "1+1=2" 2 (1+1))

tests = TestList [testAddition]



add :: Int -> Int -> Int
add x y = x + y

main :: IO ()
main = do
  -- Run the unit tests
  
  runTestTT tests

  putStrLn "Now calling the main function from Graph module:"
  Graph.main
  
  -- Call the `add` function and print the result
  let result = add 1 2
  print result
  print()
