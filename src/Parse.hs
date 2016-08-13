{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Parse where

import Control.Applicative

import Text.Parsec hiding (many, optional, (<|>), for_)

import Tokens


whole = many1 (text <|> img <|> div_p)

text = Text <$> ((string "text") *> spaces *> attr <* spaces)

img = Img <$>
    ((string "img") *> spaces *> (string ".") *> spaces *> (string "src") *> spaces *> attr) <*>
    (spaces *> (string ".") *> spaces *> (string "alt") *> spaces *> attr)

attr = (string "\"") *> (manyTill anyChar (char '\"'))

div_p = Div <$> ((string "div") *> spaces *> (string "{") *> spaces *> whole <* spaces <* (string "}") <* spaces)
