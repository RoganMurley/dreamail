module Tokens where

data Token =  Div Class [Token] | Text String | Img Src Alt
   deriving (Show)

type Src   = String
type Alt   = String
type Class = String
