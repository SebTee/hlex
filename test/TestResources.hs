module TestResources
     ( assertLexResult
     , assertLexException
     ) where

import Test.HUnit
import Hlex

assertLexResult :: (Eq a, Show a) => Lexer a -> String -> [a] -> IO()
assertLexResult lexer program expectedTokens = case lexer program of
  Right tokens -> assertEqual "Incorrectly parsed" expectedTokens tokens
  Left err -> assertFailure $ "Failed with the following exception " ++ (show err)

assertLexException :: (Show a) => Lexer a -> String -> LexException -> IO ()
assertLexException lexer program expectedException = case lexer program of
  Right tokens -> assertFailure $ "Successfully parsed to: " ++ show tokens
  Left err -> assertEqual "Incorrect exception" expectedException err
