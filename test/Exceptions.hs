module Exceptions (exceptions) where

import Test.HUnit
import Hlex
import TestResources
import ExampleLang

exceptions :: Test
exceptions = TestList [ TestLabel "Location Exception" exceptionLocation
                      , TestLabel "Number Parse Exception" exceptionNumParse
                      , TestLabel "String Parse Exception" exceptionStrParse
                      ]

exceptionLocation :: Test
exceptionLocation = TestCase $ assertLexException lexer "aaaa\n\naaaaa\naaa\naaa//bbbaa\naaaaa" $ UnmatchedException 5 4 "//"

exceptionNumParse :: Test
exceptionNumParse = TestCase $ assertLexException lexer "10.2.3" $ UnmatchedException 1 5 "."

exceptionStrParse :: Test
exceptionStrParse = TestCase $ assertLexException lexer "\nab\"cd\n\"ef" $ MatchedException 2 3 "\"cd\n" "Can't have a new line in a string"