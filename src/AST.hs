module AST where

import Data.Map.Strict as Map


data Doc = Doc Stylesheet [Row]
    deriving (Show)

-- Body.
data Body =
      Div Class [Body]
    | Img Src Alt Class
    | Heading HeadingLevel Class String
    | Text String
    | A Href Class [Body]
    | Comment String
    deriving (Show)

data Row = Row [Col]
    deriving (Show)

data Col = Col [Body] Width GutterL GutterR Position
    deriving (Show)

data Position = First | Middle | Last
    deriving (Show)

data Direction = DirTop | DirRight | DirBottom | DirLeft
    deriving (Show)

data HeadingLevel = H1 | H2 | H3 | H4 | H5 | H6
    deriving (Show)

type Alt     = String
type Class   = String
type GutterL = Int
type GutterR = Int
type Href    = String
type Src     = String
type Width   = Int


-- Stylesheet.
type Stylesheet = Map.Map Class [Style]

data Style =
      FontColor HexColor
    | FontSize Int
    | BackgroundColor HexColor
    | Width String
    | Height String
    | Padding Direction Int
    deriving (Show)

type HexColor = String

styleBase :: Stylesheet
styleBase = Map.empty

getStyles :: Class -> Stylesheet -> [Style]
getStyles c s = concat (Map.lookup c s)

addStyles :: Class -> [Style] -> Stylesheet -> Stylesheet
addStyles c xs s = Map.insert c styles s
    where
        styles :: [Style]
        styles = (getStyles c s) ++ xs
