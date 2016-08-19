{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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
line = (text <|> img <|> div_p <|> link <|> heading) <* spaces

text :: IParser Token
text = Text <$> (string "text" *> onlySpaces *> stringLiteral)

heading :: IParser Token
heading = Heading <$> ((string "h") *> levels) <*> (onlySpaces *> stringLiteral)
    where
    levels :: IParser String
    levels = string "1" <|> string "2" <|> string "3" <|> string "4" <|> string "5" <|> string "6"

img :: IParser Token
img  = Img <$> (string "img" *> onlySpaces *> attr "src") <*> (onlySpaces *> attr "alt")

link :: IParser Token
link  = withBlock A (string "a" *> onlySpaces *> (attr "href") <* spaces) line

div_p :: IParser Token
div_p = withBlock (flip (const . Div)) (string "div" <* spaces) line

col :: IParser Token
col   = withBlock (flip (const . Col)) (string "col" <* spaces) line

row :: IParser Token
row   = withBlock (flip (const . Row)) (string "row" <* spaces) col

attr :: String -> IParser String
attr name = string "." *> onlySpaces *> (string name) *> onlySpaces *> stringLiteral

stringLiteral :: IParser String
stringLiteral = (string "\"") *> (manyTill anyChar (char '\"'))

onlySpaces :: IParser String
onlySpaces = many space
