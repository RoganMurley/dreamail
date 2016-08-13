module Tokens where

data Token =  Text String | Img Src Alt
   deriving (Show)

type Src = String
type Alt = String
