import Test.HUnit
import Hlex

main :: IO Counts
main = runTestTT tests

data Token = Ident String

grammar :: Grammar Token
grammar = [ Tokenize "[a]+" Ident
          , Skip "[\n]+"
          ]

lexer :: Lexer Token
lexer = hlex grammar

tests :: Test
tests = TestList [exception]

exception :: Test
exception = TestCase $ case lexer "aaaa\n\naaaaa\naaa\naaabbbaa\naaaaa" of
    Right _ -> assertFailure "Successfully parsed"
    Left err -> assertEqual "Incorrect exception" err $ LexError 5 4 "bbb"
