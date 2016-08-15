module AST where

data AST = Row [AST] | Col [AST] Width GutterL GutterR Position | Div [AST] | Img Src Alt | H1 String | Text String
    deriving (Show)

type Alt   = String
type Src   = String
type Width = Int
type GutterL = Int
type GutterR = Int

data Position = First | Middle | Last
    deriving (Show)
