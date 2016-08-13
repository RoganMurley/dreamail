{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Parse where

import Control.Applicative

import Text.Parsec hiding (many, optional, (<|>), for_)

import Tokens


whole = many (text <|> img)

text = Text <$> ((string "Hello, World!") <* spaces)

img = Img <$> ((string "img") *> spaces *> string "." *> spaces *> (string "src") *> spaces *> string "\"" *> url <* string "\"" <* spaces)

url = many1 (alphaNum <|> char '_')
