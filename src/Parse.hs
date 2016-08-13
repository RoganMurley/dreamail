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

text = Text <$> ((string "text") *> spaces *> attr <* spaces)

img = Img <$>
    ((string "img") *> spaces *> (string ".") *> spaces *> (string "src") *> spaces *> attr) <*>
    (spaces *> (string ".") *> spaces *> (string "alt") *> spaces *> attr)

attr = (string "\"") *> (manyTill anyChar (char '\"'))

div_p = withBlock Div (string "div" *> spaces *> string "." *> spaces *> string "class" *> spaces *> attr <* spaces) line
