import Test.HUnit
import qualified DependencyGraph.Graph as Graph
import System.Environment (withArgs)
import System.IO.Silently (capture_)

testAddition :: Test
testAddition = TestCase (assertEqual "1+1=2" 2 (1+1))

testGraphMain :: Test
testGraphMain = TestCase $ do
  expectedOutput <- readFile "Tests/pure_expected.txt"
  actualOutput <- capture_ $ withArgs ["Tests/pure.hs"] Graph.main
  assertEqual "The output of graphMain is not as expected" expectedOutput actualOutput

tests :: Test
tests = TestList [testAddition, testGraphMain]

add :: Int -> Int -> Int
add x y = x + y

main :: IO ()
main = do
  -- Run the unit tests
  _ <- runTestTT tests

  putStrLn "Test Complete"

