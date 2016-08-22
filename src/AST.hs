module AST where

import Data.Map.Strict as Map


-- Body AST
data AST = Div Class [AST] | Img Src Alt Class | Heading HeadingLevel Class String | Text String | A Href Class [AST] | Comment String
    deriving (Show)

type Alt     = String
type Class   = String
type GutterL = Int
type GutterR = Int
type Href    = String
type Src     = String
type Width   = Int

data Position = First | Middle | Last
    deriving (Show)

data Root = Root [Row] Stylesheet
    deriving (Show)

data Row = Row [Col]
    deriving (Show)

data Col = Col [AST] Width GutterL GutterR Position
    deriving (Show)

data HeadingLevel = H1 | H2 | H3 | H4 | H5 | H6
    deriving (Show)

-- Style AST.
type HexColor = String
type Stylesheet = Map.Map Class [Style]
data Style = TextColor HexColor
    deriving (Show)

styleBase :: Stylesheet
styleBase = Map.insert "header" [TextColor "#123fff"] Map.empty

getStyles :: Class -> Stylesheet -> [Style]
getStyles c s = maybeToList (Map.lookup c s)
    where
    maybeToList :: Maybe [a] -> [a]
    maybeToList (Just xs) = xs
    maybeToList Nothing = []
