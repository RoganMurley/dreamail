{-# LANGUAGE OverloadedStrings #-}

module Translate where

import Control.Monad

import Text.Blaze.Html4.Transitional as H
import Text.Blaze.Html4.Transitional.Attributes as A
import Text.Blaze.Internal (attribute, Attribute, AttributeValue)

import Tokens


template :: Html -> Html
template b = docTypeHtml $ do
    H.head $ do
        meta ! httpEquiv "Content-Type"    ! content "text/html; charset=UTF-8"
        meta ! name "viewport"             ! content "width=device-width, initial-scale=1" -- So that mobile will display zoomed in.
        meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge"                             -- Enable media queries for windows phone 8.
        meta ! name "format-detection"     ! content "telephone=no"                        -- Disable auto telephone linking in iOS.
        H.title "Made with Dreamail"
    body ! bgcolor "#ffffff" ! leftmargin "0" ! topmargin "0" ! marginwidth "0" ! marginheight "0" $ do
        table ! width "600" ! border "0" ! cellpadding "0" ! cellspacing "0" ! align "center" $
            tr $
                td $
                    b

translate :: [Token] -> Html
translate xs = template $ forM_ xs translateEach

translateEach :: Token -> Html
translateEach (Text x)  = string x
translateEach (Img s a) = img ! src (toValue s) ! alt (toValue a)
translateEach (Div c xs)  = H.div ! class_ (toValue c) $ forM_ xs translateEach


leftmargin :: AttributeValue -> Attribute
leftmargin = attribute "leftmargin" " leftmargin=\""

topmargin :: AttributeValue -> Attribute
topmargin = attribute "topmargin" " topmargin=\""

marginwidth :: AttributeValue -> Attribute
marginwidth = attribute "marginwidth" " marginwidth=\""

marginheight :: AttributeValue -> Attribute
marginheight = attribute "marginheight" " marginheight=\""
