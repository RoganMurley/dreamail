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


whole = block line <* eof

line = (text <|> img <|> div_p <|> row) <* spaces

text = Text <$> (string "text" *> onlySpaces *> stringLiteral)

img  = Img <$> (string "img" *> onlySpaces *> attr "src") <*> (onlySpaces *> attr "alt")

div_p = withBlock (flip (const . Div)) (string "div" <* spaces) line

row   = withBlock (flip (const . Row)) (string "row" <* spaces) line

attr name = string "." *> onlySpaces *> (string name) *> onlySpaces *> stringLiteral

stringLiteral = (string "\"") *> (manyTill anyChar (char '\"'))

onlySpaces = many space
