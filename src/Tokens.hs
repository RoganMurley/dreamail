module Tokens where

data Token =  Div [Token] | Text String | Img Src Alt
   deriving (Show)

type Src = String
type Alt = String
