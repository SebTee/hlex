module Exceptions (exceptions) where

import Test.HUnit
import Hlex
import TestResources
import ExampleLang

exceptions :: Test
exceptions = TestList [ TestLabel "Location Exception" exceptionLocation
                      , TestLabel "Number Parse Exception" exceptionNumParse
                      ]

exceptionLocation :: Test
exceptionLocation = TestCase $ assertLexException lexer "aaaa\n\naaaaa\naaa\naaa///bbbaa\naaaaa" $ LexError 5 4 "///"

exceptionNumParse :: Test
exceptionNumParse = TestCase $ assertLexException lexer "10.2.3" $ LexError 1 3 "."