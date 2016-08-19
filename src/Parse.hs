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

whole :: IParser [Token]
whole = block row <* eof

line :: IParser Token
line = (text <|> img <|> div_p <|> link <|> heading <|> comment) <* spaces

text :: IParser Token
text = Text <$> (string "text" *> onlySpaces *> stringLiteral)

heading :: IParser Token
heading = Heading <$>
    (string "h" *> levels) <*>
    (onlySpaces *> optClass) <*>
    (onlySpaces *> stringLiteral)
    where
    levels :: IParser String
    levels = string "1" <|> string "2" <|> string "3" <|> string "4" <|> string "5" <|> string "6"

img :: IParser Token
img = Img <$>
    (string "img" *> onlySpaces *> attr "src") <*>
    (onlySpaces *> attr "alt") <*>
    (onlySpaces *> optClass)

link :: IParser Token
link = withBlock tupA
    (mkTup <$> (string "a" *> onlySpaces *> attr "href") <*> (onlySpaces *> optClass <* spaces))
    line
    where
    mkTup :: a -> b -> (a, b)
    mkTup a b = (a, b)
    tupA :: (String, String) -> [Token] -> Token
    tupA (a, b) = A a b

div_p :: IParser Token
div_p = withBlock Div
    (string "div" *> onlySpaces *> optClass <* spaces)
    line

comment :: IParser Token
comment = Comment <$>
    (string "//" *> manyTill anyChar newline)

col :: IParser Token
col = withBlock
    (flip (const . Col)) (string "col" <* spaces)
    line

row :: IParser Token
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
