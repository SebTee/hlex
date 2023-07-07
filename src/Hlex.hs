{-|
Module      : Hlex
Description : Lexer creation tools
Copyright   : (c) Sebastian Tee, 2023
License     : MIT

Tools needed to create a 'Lexer' from a lexical 'Grammar'.
-}
module Hlex
     ( -- * Example
       -- $example

       -- * Types
       Grammar
     , TokenSyntax(..)
     , Lexer
       -- ** Exceptions
     , LexException(..)
       -- * Functions
     , hlex
     ) where

import Text.Regex.TDFA ((=~))
import Data.Maybe (maybeToList)

-- | Exception thrown when a 'Lexer' encounters an error when lexxing a string.
data LexException 
  = UnmatchedException -- ^ Exception thrown when a substring cannot be matched.
    Int -- ^ The line number where the substring that couldn't be lexed is located.
    Int -- ^ The column where the substring that couldn't be lexed is located.
    String -- ^ The subtring that couldn't be lexed.
  | MatchedException -- ^ Exception thrown when a macth is found on the 'Error' 'TokenSyntax'.
    Int -- ^ The line number where the matched string is located.
    Int -- ^ The column where the matched string is located.
    String -- ^ The matched string.
    String -- ^ Error message.
  deriving(Read, Show, Eq)

-- | These are the individual rules that make up a 'Grammar'.
--
-- Takes a __POSIX regular expression__ then converts it to a token or skips it.
data TokenSyntax token
  = Skip -- ^ Skips over any matches.
    String -- ^ Regular expression.
  | Tokenize -- ^ Takes a function that converts the matched string to a token.
    String -- ^ Regular expression.
    (String -> token) -- ^ Function that converts the matched string into a token.
  | JustToken -- ^ Converts any regular expression matches to a given token.
    String -- ^ Regular expression.
    token -- ^ Given token.
  | Error -- ^ Returns an error with a message when a match occurs.
    String -- ^ Regular expression.
    String -- ^ Error message.

-- | Lexical grammar made up of 'TokenSyntax' rules.
--
-- The __order is important__. The 'Lexer' will apply each 'TokenSyntax' rule in the order listed.
type Grammar token = [TokenSyntax token]

-- | Converts a string into a list of tokens.
-- If the string does not follow the Lexer's 'Grammar' a 'LexException' will be returned.
type Lexer token = String -> Either LexException [token]

-- | Takes a given 'Grammar' and turns it into a 'Lexer'.
hlex :: Grammar token -> Lexer token
hlex = hlex' 1 1

hlex' :: Int -> Int -> Grammar token -> Lexer token
hlex' _ _ _ [] = Right []
hlex' row col tzss@(tz:tzs) program =
  if null matchedText
  then hlex' row col tzs program
  else case tz of
    Error _ errMessage -> Left $ uncurry MatchedException (getLastCharPos row col beforeProgram) matchedText errMessage
    Skip _ -> lexCont Nothing
    Tokenize _ f -> lexCont $ Just $ f matchedText
    JustToken _ token -> lexCont $ Just token
  where
    (beforeProgram, matchedText, afterProgram) = program =~ getRegex tz :: (String, String, String)
    lexCont t = do
      before <- hlex' row col tzs beforeProgram
      after <- uncurry hlex' (getLastCharPos row col (beforeProgram ++ matchedText)) tzss afterProgram
      Right $ before ++ maybeToList t ++ after
hlex' row col _ invalidString = Left $ UnmatchedException row col invalidString

getLastCharPos :: Int -> Int -> String -> (Int, Int)
getLastCharPos startRow startCol x = (startRow + addRow, addCol + if addRow == 0 then startCol else 1)
  where
    ls = lines x
    addRow = length ls - 1
    addCol = length $ last ls

getRegex :: TokenSyntax token -> String
getRegex (Skip regex) = regex
getRegex (Tokenize regex _) = regex
getRegex (JustToken regex _) = regex
getRegex (Error regex _) = regex

{- $example
Here is an example module for a simple language.

@
  module ExampleLang
       ( MyToken(..) -- Export the language's tokens and the lexer
       , myLexer
       ) where

  import Hlex

  data MyToken = Ident String -- String identifier token
               | Number Float -- Number token and numeric value
               | Assign       -- Assignment operator token
               deriving(Show)

  myGrammar :: Grammar MyToken
  myGrammar = [ JustToken "=" Assign                                       -- "=" Operator becomes the assign token
              , Tokenize "[a-zA-Z]+" (\match -> Ident match)                -- Identifier token with string
              , Tokenize "[0-9]+(\\.[0-9]+)?" (\match -> Number (read match) -- Number token with the parsed numeric value stored as a Float
              , Skip "[ \\n\\r\\t]+"                                          -- Skip whitespace
              ]

  myLexer :: Lexer MyToken
  myLexer = hlex myGrammar -- hlex turns a Grammar into a Lexer
@

Here is the lexer being used on a simple program.

>>> lexer "x = 1.2"
Right [Ident "x", Assign, Number 1.2]

The lexer uses 'Either'. Right means the lexer successfully parsed the program to a list of MyTokens.
If Left was returned it would be a 'LexException'.
-}
