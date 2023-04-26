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

-- | Exception thrown when a 'Lexer' is unable to lex a string.
data LexException = LexException
  Int -- ^ The line number where the string that couldn't be lexed is located.
  Int -- ^ The column where the string that couldn't be lexed is located.
  String -- ^ The String that couldn't be lexed.
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

type InternalToken token = (String, Maybe (String -> token))

-- | Lexical grammar made up of 'TokenSyntax' rules.
--
-- The __order is important__. The 'Lexer' will apply each 'TokenSyntax' rule in the order listed.
type Grammar token = [TokenSyntax token]

-- | Converts a string into a list of tokens.
-- If the string does not follow the Lexer's 'Grammar' a 'LexException' will be returned.
type Lexer token = String -> Either LexException [token]

tokenizerToInternalToken :: TokenSyntax a -> InternalToken a
tokenizerToInternalToken (Skip regex) = (regex, Nothing)
tokenizerToInternalToken (Tokenize regex toToken) = (regex, Just toToken)
tokenizerToInternalToken (JustToken regex token) = (regex, Just $ \_ -> token)

-- | Takes a given 'Grammar' and turns it into a 'Lexer'.
hlex :: Grammar token -> Lexer token
hlex = lexInternal 1 1 . map tokenizerToInternalToken

lexInternal :: Int -> Int -> [InternalToken token] -> Lexer token
lexInternal _ _ _ "" = Right []
lexInternal row col ((regex, t):grammar) program = if null matchedText
  then lexInternal row col grammar program
  else do
    before <- parsedBefore
    after <- parsedAfter
    case t of
      Nothing -> Right $ before ++ after
      Just tk -> Right $ before ++ tk matchedText : after
  where
    (beforeProgram, matchedText, afterProgram) = program =~ regex :: (String, String, String)
    (afterRow, afterCol) = getLastCharPos row col (beforeProgram ++ matchedText)
    parsedBefore = lexInternal row col grammar beforeProgram
    parsedAfter = lexInternal afterRow afterCol ((regex, t):grammar) afterProgram
lexInternal row col _ invalidString = Left $ LexException row col invalidString

getLastCharPos :: Int -> Int -> String -> (Int, Int)
getLastCharPos startRow startCol x = (startRow + addRow, addCol + if addRow == 0 then startCol else 1)
  where
    ls = lines x
    addRow = length ls - 1
    addCol = length $ last ls

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
  myGrammar = [ JustToken "=" Assign                                     -- "=" Operator becomes the assign token
            , Tokenize "[a-zA-Z]+" (\match -> Ident match)                -- Identifier token with string
            , Tokenize "[0-9]+(\\.[0-9]+)?" (\match -> Number (read match) -- Number token with the parsed numeric value stored as a Float
            , Skip "[ \n\r\t]+"                                             -- Skip whitespace
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
