module Tokens where

data Token =  Row [Token] | Col [Token] | Div [Token] | Img Src Alt | H1 String | Text String
   deriving (Show)

type Alt   = String
type Src   = String
