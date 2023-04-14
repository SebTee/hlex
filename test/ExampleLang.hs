module ExampleLang
     ( Token(..)
     , lexer
     ) where

import Hlex

data Token = Ident String
           | Number Float
           | Assign
           deriving(Read, Show, Eq)

grammar :: Grammar Token
grammar = [ JustToken "=" Assign
          , Tokenize "[a-zA-Z]+" Ident
          , Tokenize "[0-9]+(\\.[0-9]+)?" $ Number . read
          , Skip "[ \n\r\t]+"
          ]

lexer :: Lexer Token
lexer = hlex grammar