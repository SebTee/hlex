import System.Exit
import Test.HUnit
import Exceptions
import Successes

main :: IO ()
main = do
  results <- runTestTT tests
  case results of
    Counts {errors = 0, failures = 0} -> exitSuccess
    _ -> exitFailure

tests :: Test
tests = TestList [TestLabel "Successes" successes, TestLabel "Exceptions" exceptions]
