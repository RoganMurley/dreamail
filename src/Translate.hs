{-# LANGUAGE OverloadedStrings #-}

module Translate where

import Control.Monad

import Text.Blaze.Html4.Transitional as H
import Text.Blaze.Html4.Transitional.Attributes as A
import Text.Blaze.Internal (attribute, Attribute, AttributeValue)

import AST


template :: Html -> Html
template b = docTypeHtml $ do
    H.head $ do
        meta ! httpEquiv "Content-Type"    ! content "text/html; charset=UTF-8"
        meta ! name "viewport"             ! content "width=device-width, initial-scale=1" -- So that mobile will display zoomed in.
        meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge"                             -- Enable media queries for windows phone 8.
        meta ! name "format-detection"     ! content "telephone=no"                        -- Disable auto telephone linking in iOS.
        H.title "Made with Dreamail"
        H.style "/* Level Two: reset mobile gmail first gutters */\
            \.col-first-td {\
              \padding-left:  20px !important;\
              \padding-right: 10px !important;\
            \}\
            \.col-td {\
              \padding-left:  10px !important;\
              \padding-right: 10px !important;\
            \}\
            \.col-last-td {\
              \padding-left:  10px !important;\
              \padding-right: 20px !important;\
            \}\
            \/* Level Three: use mobile gutter */\
            \@media screen and (max-width: 599px) {\
              \.col {\
                \width: 100% !important;\
                \max-width: 100% !important;\
              \}\
              \.col td {\
                \padding-left:  10px !important;\
                \padding-right: 10px !important;\
              \}\
            \}"
    body ! bgcolor "#ffffff" ! leftmargin "0" ! topmargin "0" ! marginwidth "0" ! marginheight "0" $ b

translate :: [AST] -> Html
translate xs = template $ forM_ xs translateEach

translateEach :: AST -> Html
translateEach (Text x)   = string x
translateEach (Img s a)  = img ! src (toValue s) ! alt (toValue a)
translateEach (Div xs)   = H.div $ forM_ xs translateEach
translateEach (Col xs)   = do
    preEscapedToHtml ("<!--[if mso]><table width=\"100%\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\"><tr><td valign=\"top\" width=\"300\"><![endif]-->" :: String)
    table ! border "0" ! cellpadding "0" ! cellspacing "0" ! align "left" ! class_ "col" ! A.style "width:100%;max-width:300px;" $
        tr $
            td ! class_ "col-first-td" ! A.style "padding-left:20px;padding-right:20px;" $
                forM_ xs translateEach
    preEscapedToHtml ("<!--[if mso]></td><td valign=\"top\" width=\"300\"><![endif]-->" :: String)
translateEach (Row xs)   =
    table ! width "600" ! border "0" ! cellpadding "0" ! cellspacing "0" ! align "center" $ -- TODO: ADD OUTLOOK WRAPPER TABLE!!!
        tr $
            td $
                forM_ xs translateEach


leftmargin :: AttributeValue -> Attribute
leftmargin = attribute "leftmargin" " leftmargin=\""

topmargin :: AttributeValue -> Attribute
topmargin = attribute "topmargin" " topmargin=\""

marginwidth :: AttributeValue -> Attribute
marginwidth = attribute "marginwidth" " marginwidth=\""

marginheight :: AttributeValue -> Attribute
marginheight = attribute "marginheight" " marginheight=\""
