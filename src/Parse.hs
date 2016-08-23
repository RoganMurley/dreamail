{-# LANGUAGE PackageImports #-}

module Parse where

import Control.Applicative
import "mtl" Control.Monad.State

import Text.Parsec hiding (many, optional, (<|>), for_, State)
import Text.Parsec.Indent

import Tokens


type IParser a = ParsecT String () (State SourcePos) a

iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser source_name input =
    runIndent source_name $ runParserT aParser () source_name input

doc :: IParser Doc
doc = Doc <$> style <*> body <* eof

style :: IParser [StyleBlock]
style = (withBlock' (string "style" <* onlySpaces) classStyle) <|> (pure [])

classStyle :: IParser StyleBlock
classStyle = withBlock
    ClassBlock
    (string "." *> manyTill anyChar newline <* onlySpaces)
    textCol

textCol :: IParser Style
textCol = TextColor <$> (string "text-color" *> onlySpaces *> hexCol <* spaces)
    where
    hexCol :: IParser String
    hexCol = string "#" *> count 6 hexDigit <* newline

body :: IParser [Body]
body = withBlock' (string "body" <* onlySpaces) row

line :: IParser Body
line = (text <|> img <|> div_p <|> link <|> heading <|> comment) <* spaces

text :: IParser Body
text = Text <$> (string "text" *> onlySpaces *> stringLiteral)

heading :: IParser Body
heading = Heading <$>
    (string "h" *> levels) <*>
    (onlySpaces *> optClass) <*>
    (onlySpaces *> stringLiteral)
    where
    levels :: IParser String
    levels = string "1" <|> string "2" <|> string "3" <|> string "4" <|> string "5" <|> string "6"

img :: IParser Body
img = Img <$>
    (string "img" *> onlySpaces *> attr "src") <*>
    (onlySpaces *> attr "alt") <*>
    (onlySpaces *> optClass)

link :: IParser Body
link = withBlock tupA
    (mkTup <$> (string "a" *> onlySpaces *> attr "href") <*> (onlySpaces *> optClass <* spaces))
    line
    where
    mkTup :: a -> b -> (a, b)
    mkTup a b = (a, b)
    tupA :: (String, String) -> [Body] -> Body
    tupA (a, b) = A a b

div_p :: IParser Body
div_p = withBlock Div
    (string "div" *> onlySpaces *> optClass <* spaces)
    line

comment :: IParser Body
comment = Comment <$>
    (string "//" *> manyTill anyChar newline)

col :: IParser Body
col = withBlock
    (flip (const . Col)) (string "col" <* spaces)
    line

row :: IParser Body
row = withBlock
    (flip (const . Row)) (string "row" <* spaces)
    col

attr :: String -> IParser String
attr name = string "." *> onlySpaces *> string name *> onlySpaces *> stringLiteral

optClass :: IParser String
optClass = attr "class" <|> pure ""

stringLiteral :: IParser String
stringLiteral = string "\"" *> manyTill anyChar (char '\"')

onlySpaces :: IParser String
onlySpaces = many space
