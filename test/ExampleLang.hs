module ExampleLang
     ( Token(..)
     , lexer
     ) where

import Hlex

data Token = Ident String
           | Str String
           | Number Float
           | Assign
           deriving(Read, Show, Eq)

grammar :: Grammar Token
grammar = [ Error "\"[^\"]*\n" "Can't have a new line in a string"
          , Tokenize "\"[^\"]*\"" $ Str . init . tail
          , JustToken "=" Assign
          , Tokenize "[a-zA-Z]+" Ident
          , Tokenize "[0-9]+(\\.[0-9]+)?" $ Number . read
          , Skip "[ \n\r\t]+"
          ]

lexer :: Lexer Token
lexer = hlex grammar