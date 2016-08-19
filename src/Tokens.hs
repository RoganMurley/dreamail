module Tokens where

data Token =  Row [Token] | Col [Token] | Div [Token] | Img Src Alt | Heading String String | H2 String | Text String | A String [Token]
   deriving (Show)

type Alt   = String
type Src   = String
