module Tokens where

data Token =  Row [Token] | Col [Token] | Div Class [Token] | Img Src Alt Class | Heading String Class String | Text String | A Href Class [Token] | Comment String
   deriving (Show)

type Alt   = String
type Class = String
type Href  = String
type Src   = String
