import Test.HUnit
import qualified DependencyGraph.Graph as Graph
import System.Environment (withArgs)
import System.IO.Silently (capture_)

testAddition :: Test
testAddition = TestCase (assertEqual "1+1=2" 2 (1+1))

testPure1 :: Test
testPure1 = TestCase $ do
  expectedOutput <- readFile "Tests/pure1_expected.txt"
  actualOutput <- capture_ $ withArgs ["Tests/pure1.hs"] Graph.main
  assertEqual "The output of Pure1 is not as expected" expectedOutput actualOutput

testPure2 :: Test
testPure2 = TestCase $ do
  expectedOutput <- readFile "Tests/pure2_expected.txt"
  actualOutput <- capture_ $ withArgs ["Tests/pure2.hs"] Graph.main
  assertEqual "The output of Pure1 is not as expected" expectedOutput actualOutput

testPure3 :: Test
testPure3 = TestCase $ do
  expectedOutput <- readFile "Tests/pure3_expected.txt"
  actualOutput <- capture_ $ withArgs ["Tests/pure3.hs"] Graph.main
  assertEqual "The output of Pure1 is not as expected" expectedOutput actualOutput

tests :: Test
tests = TestList [testPure1 , testPure2, testPure3]


main :: IO ()
main = do
  -- Run the unit tests
  _ <- runTestTT tests

  putStrLn "Dependency Graph Test Complete"

