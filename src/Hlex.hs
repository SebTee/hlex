module Hlex
     ( Grammar
     , Lexer
     , LexError(..)
     , Tokenizer(..)
     , hlex
     ) where

import Text.Regex.TDFA
import Data.List.Split (splitOn)

data LexError = LexError Int Int String
  deriving(Read, Show, Eq)

data Tokenizer a = Skip String
                 | Tokenize String (String -> a)
                 | JustToken String a

type InternalToken a = (String, Maybe (String -> a))

type Grammar a = [Tokenizer a]

type Lexer a = String -> Either LexError [a]

tokenizerToInternalToken :: Tokenizer a -> InternalToken a
tokenizerToInternalToken (Skip regex) = (regex, Nothing)
tokenizerToInternalToken (Tokenize regex toToken) = (regex, Just toToken)
tokenizerToInternalToken (JustToken regex token) = (regex, Just $ \_ -> token)

hlex :: Grammar a -> Lexer a
hlex grammar program = case lexInternal (map tokenizerToInternalToken grammar) program of
  Left invalidString -> Left $ (uncurry LexError $ findSubstringColRow invalidString program) invalidString
  Right tokens -> Right tokens

lexInternal :: [InternalToken a] -> String -> Either String [a]
lexInternal _ "" = Right []
lexInternal ((regex, t):grammar) program = case matchedText of
  "" -> lexInternal grammar program
  _ -> case (parsedBefore, parsedAfter) of
    (Right b, Right a) -> case t of
      Nothing -> Right $ b ++ a
      Just tk -> Right $ b ++ tk matchedText : a
    (Left ex, _) -> Left ex
    (_, Left ex) -> Left ex
  where
    (beforeProgram, matchedText, afterProgram) = program =~ regex :: (String, String, String)
    parsedBefore = lexInternal grammar beforeProgram
    parsedAfter = lexInternal ((regex, t):grammar) afterProgram
lexInternal [] invalidString = Left $ invalidString

findSubstringColRow :: String -> String -> (Int, Int)
findSubstringColRow subStr str = (lineNo, colNo)
  where
    prev = head $ splitOn subStr str
    prevLines = lines prev
    lineNo = length prevLines
    colNo = 1 + (length $ last prevLines)
