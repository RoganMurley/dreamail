module Tokens where

data Token =  Text String | Img Src
   deriving (Show)

type Src = String
