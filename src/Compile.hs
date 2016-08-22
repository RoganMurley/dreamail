{-# LANGUAGE OverloadedStrings #-}

module Compile where

import Control.Monad
import Data.Maybe

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

compile :: Root -> Html
compile (Root xs s) = template $ forM_ xs (compileRow s)

compileRow :: Stylesheet -> Row -> Html
compileRow s (Row xs) =
    table ! width "600" ! border "0" ! cellpadding "0" ! cellspacing "0" ! align "center" $ -- TODO: ADD OUTLOOK WRAPPER TABLE!!!
        tr $
            td $
                forM_ xs (compileCol s)

compileCol :: Stylesheet -> Col -> Html
compileCol s (Col xs w gl gr pos) = do
    preEscapedToHtml ("<!--[if mso]><table width=\"100%\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\"><tr><td valign=\"top\" width=\"" ++ (show w) ++ "\"><![endif]-->" :: String)
    table ! border "0" ! cellpadding "0" ! cellspacing "0" ! align "left" ! class_ "col" ! A.style (toValue ("width:100%;max-width:" ++ (show w) ++ "px;")) $
        tr $
            td ! class_ (colClass pos) ! A.style (toValue ("padding-left:" ++ (show gl) ++ "px;padding-right:" ++ (show gr) ++ "px;")) $
                forM_ xs (compileEach s)
    preEscapedToHtml ("<!--[if mso]></td><td valign=\"top\" width=\"" ++ (show w) ++ "\"><![endif]-->" :: String)
    where
        colClass First  = "col-first-td"
        colClass Middle = "col-td"
        colClass Last   = "col-last-td"

compileEach :: Stylesheet -> AST -> Html
compileEach s (Text x)         = string x
compileEach s (Img sr a c)     = img ! src (toValue sr) ! alt (toValue a) ! class_ (toValue c) ! inlineStyle s c
compileEach s (Div c xs)       = H.div ! class_ (toValue c) ! inlineStyle s c $ forM_ xs (compileEach s)
compileEach s (A u c xs)       = H.a ! href (toValue u) ! class_ (toValue c) ! inlineStyle s c $ forM_ xs (compileEach s)
compileEach s (Comment _)      = string ""
compileEach s (Heading H1 c x) = h1 ! class_ (toValue c) ! inlineStyle s c $ string x
compileEach s (Heading H2 c x) = h2 ! class_ (toValue c) ! inlineStyle s c $ string x
compileEach s (Heading H3 c x) = h3 ! class_ (toValue c) ! inlineStyle s c $ string x
compileEach s (Heading H4 c x) = h4 ! class_ (toValue c) ! inlineStyle s c $ string x
compileEach s (Heading H5 c x) = h5 ! class_ (toValue c) ! inlineStyle s c $ string x
compileEach s (Heading H6 c x) = h6 ! class_ (toValue c) ! inlineStyle s c $ string x

leftmargin :: AttributeValue -> Attribute
leftmargin = attribute "leftmargin" " leftmargin=\""

topmargin :: AttributeValue -> Attribute
topmargin = attribute "topmargin" " topmargin=\""

marginwidth :: AttributeValue -> Attribute
marginwidth = attribute "marginwidth" " marginwidth=\""

marginheight :: AttributeValue -> Attribute
marginheight = attribute "marginheight" " marginheight=\""

-- Style stuff!
inlineStyle :: Stylesheet -> Class -> Attribute
inlineStyle s c = A.style $ toValue $ compileStyles $ getStyles c s
    where
    compileStyles :: [Style] -> String
    compileStyles = concatMap compileStyle

compileStyle :: Style -> String
compileStyle (TextColor c) = "color:" ++ c ++ ";"
