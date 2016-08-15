module AST where

data AST = Row [AST] | Col [AST] | Div [AST] | Img Src Alt | Text String
    deriving (Show)

type Src   = String
type Alt   = String
