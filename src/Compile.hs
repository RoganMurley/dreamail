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
compile (Root xs _) = template $ forM_ xs compileRow

compileRow :: Row -> Html
compileRow (Row xs) =
    table ! width "600" ! border "0" ! cellpadding "0" ! cellspacing "0" ! align "center" $ -- TODO: ADD OUTLOOK WRAPPER TABLE!!!
        tr $
            td $
                forM_ xs compileCol

compileCol :: Col -> Html
compileCol (Col xs w gl gr pos) = do
    preEscapedToHtml ("<!--[if mso]><table width=\"100%\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\"><tr><td valign=\"top\" width=\"" ++ (show w) ++ "\"><![endif]-->" :: String)
    table ! border "0" ! cellpadding "0" ! cellspacing "0" ! align "left" ! class_ "col" ! A.style (toValue ("width:100%;max-width:" ++ (show w) ++ "px;")) $
        tr $
            td ! class_ (colClass pos) ! A.style (toValue ("padding-left:" ++ (show gl) ++ "px;padding-right:" ++ (show gr) ++ "px;")) $
                forM_ xs compileEach
    preEscapedToHtml ("<!--[if mso]></td><td valign=\"top\" width=\"" ++ (show w) ++ "\"><![endif]-->" :: String)
    where
        colClass First  = "col-first-td"
        colClass Middle = "col-td"
        colClass Last   = "col-last-td"

compileEach :: AST -> Html
compileEach (Text x)         = string x
compileEach (Img s a c)      = img ! src (toValue s) ! alt (toValue a) ! class_ (toValue c) ! inlineStyle styleBase c
compileEach (Div c xs)       = H.div ! class_ (toValue c) $ forM_ xs compileEach
compileEach (A u c xs)       = H.a ! href (toValue u) ! class_ (toValue c) $ forM_ xs compileEach
compileEach (Comment s)      = string ""
compileEach (Heading H1 c x) = h1 ! class_ (toValue c) $ string x
compileEach (Heading H2 c x) = h2 ! class_ (toValue c) $ string x
compileEach (Heading H3 c x) = h3 ! class_ (toValue c) $ string x
compileEach (Heading H4 c x) = h4 ! class_ (toValue c) $ string x
compileEach (Heading H5 c x) = h5 ! class_ (toValue c) $ string x
compileEach (Heading H6 c x) = h6 ! class_ (toValue c) $ string x

leftmargin :: AttributeValue -> Attribute
leftmargin = attribute "leftmargin" " leftmargin=\""

topmargin :: AttributeValue -> Attribute
topmargin = attribute "topmargin" " topmargin=\""

marginwidth :: AttributeValue -> Attribute
marginwidth = attribute "marginwidth" " marginwidth=\""

marginheight :: AttributeValue -> Attribute
marginheight = attribute "marginheight" " marginheight=\""

inlineStyle :: Stylesheet -> Class -> Attribute
inlineStyle s c = A.style $ toValue $ maybeToEmptyString $ Nothing
    where
    maybeToEmptyString :: Maybe String -> String
    maybeToEmptyString (Just s)  = s
    maybeToEmptyString Nothing = ""
