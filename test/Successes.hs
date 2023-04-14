module Successes (successes) where

import Test.HUnit
import TestResources
import ExampleLang

successes :: Test
successes = TestList [ TestLabel "Parse Number" parseNum
                     , TestLabel "Assign Number" assignNumber
                     ]

parseNum :: Test
parseNum = TestCase $ assertLexResult lexer "12 3.5" [Number 12, Number 3.5]

assignNumber :: Test
assignNumber = TestCase $ assertLexResult lexer "x = 1" [Ident "x", Assign, Number 1]