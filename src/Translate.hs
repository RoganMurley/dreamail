{-# LANGUAGE OverloadedStrings #-}

module Translate where

import Control.Monad

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Tokens


template :: Html -> Html
template b = docTypeHtml $ do
    H.head $ do
        H.title "Made with Dreamail"
    body $ do
        b

translate :: [Token] -> Html
translate xs = template $ forM_ xs translateEach

translateEach :: Token -> Html
translateEach (Text x)  = string x
translateEach (Img s a) = img ! src (toValue s) ! alt (toValue a)
translateEach (Div c xs)  = H.div ! class_ (toValue c) $ forM_ xs translateEach
