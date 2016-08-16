module AST where

data AST = Div [AST] | Img Src Alt | H1 String | Text String
    deriving (Show)

type Alt   = String
type Src   = String
type Width = Int
type GutterL = Int
type GutterR = Int

data Position = First | Middle | Last
    deriving (Show)

data Root = Root [Row]
    deriving (Show)

data Row = Row [Col]
    deriving (Show)

data Col = Col [AST] Width GutterL GutterR Position
    deriving (Show)
