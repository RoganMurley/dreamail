module Tokens where

data Doc = Doc [StyleBlock] [Body]

data StyleBlock = ClassBlock Class [Style]
    deriving (Show)

data Style =
      TextColor String
    | BackgroundColor String
    | Width String
    | Height String
    deriving (Show)

data Body =
      Row [Body]
    | Col [Body]
    | Div Class [Body]
    | Img Src Alt Class
    | Heading String Class String
    | Text String
    | A Href Class [Body]
    | Comment String
    deriving (Show)

type Alt   = String
type Class = String
type Href  = String
type Src   = String
