import Test.HUnit
import Exceptions
import Successes

main :: IO Counts
main = runTestTT tests

tests :: Test
tests = TestList [TestLabel "Successes" successes, TestLabel "Exceptions" exceptions]
