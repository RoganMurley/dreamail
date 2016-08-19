module Tokens where

data Token =  Row [Token] | Col [Token] | Div [Token] | Img Src Alt | Heading String String | H2 String | Text String | A Href [Token] | Comment String
   deriving (Show)

type Alt   = String
type Src   = String
type Href  = String
