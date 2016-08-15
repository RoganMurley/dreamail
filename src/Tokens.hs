module Tokens where

data Token =  Row [Token] | Col [Token] | Div [Token] | Img Src Alt | Text String
   deriving (Show)

type Src   = String
type Alt   = String
type Class = String
