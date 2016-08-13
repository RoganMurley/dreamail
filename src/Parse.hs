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


whole = sepEndBy line newline <* eof

line = text <|> img <|> div_p

text = Text <$> ((string "text") *> spaces *> stringLiteral <* spaces)

img = Img <$> (string "img" *> spaces *> attr "src") <*> (spaces *> attr "alt")

div_p = withBlock Div (string "div" *> spaces *> option "" (attr "class") <* spaces) line

attr name = string "." *> spaces *> (string name) *> space *> stringLiteral

stringLiteral = (string "\"") *> (manyTill anyChar (char '\"'))
