module Tokens where

data DocToken = DocToken [StyleRuleToken] [BodyToken]

data StyleRuleToken = ClassRule Class [StyleToken]
    deriving (Show)

data StyleToken = TextColor String
    deriving (Show)

data BodyToken =  Row [BodyToken] | Col [BodyToken] | Div Class [BodyToken] | Img Src Alt Class | Heading String Class String | Text String | A Href Class [BodyToken] | Comment String
   deriving (Show)

type Alt   = String
type Class = String
type Href  = String
type Src   = String
