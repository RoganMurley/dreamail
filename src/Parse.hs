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


whole = block row <* eof

line = (text <|> h1 <|> img <|> div_p <|> link) <* spaces

text = Text <$> (string "text" *> onlySpaces *> stringLiteral)

h1 = H1 <$> (string "h1" *> onlySpaces *> stringLiteral)

img  = Img <$> (string "img" *> onlySpaces *> attr "src") <*> (onlySpaces *> attr "alt")

link  = withBlock A (string "a" *> onlySpaces *> (attr "href") <* spaces) line

div_p = withBlock (flip (const . Div)) (string "div" <* spaces) line

col   = withBlock (flip (const . Col)) (string "col" <* spaces) line

row   = withBlock (flip (const . Row)) (string "row" <* spaces) col

attr name = string "." *> onlySpaces *> (string name) *> onlySpaces *> stringLiteral

stringLiteral = (string "\"") *> (manyTill anyChar (char '\"'))

onlySpaces = many space
