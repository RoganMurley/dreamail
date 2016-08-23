module Semantics where

import AST as A
import Tokens as T

import Utils (fmlMap)


semantic :: T.Doc -> A.Doc
semantic (T.Doc s xs) =
    A.Doc (semanticStyleRule s A.styleBase) (semanticRow <$> xs)

semanticRow :: T.Body -> A.Row
semanticRow (T.Row xs) = A.Row $ fmlMap
    (semanticCol (gutter, quot gutter 2) (length xs) First)
    (semanticCol (quot gutter 2, quot gutter 2) (length xs) Middle)
    (semanticCol (quot gutter 2, gutter) (length xs) Last)
    xs

semanticEach :: T.Body -> A.Body
semanticEach (T.Text x)        = A.Text x
semanticEach (T.Img s a c)     = A.Img s a c
semanticEach (T.Div c xs)      = A.Div c (semanticEach <$> xs)
semanticEach (T.A u c xs)      = A.A u c (semanticEach <$> xs)
semanticEach (T.Comment s)     = A.Comment s
semanticEach (T.Heading l c x) = A.Heading (toLevel l) c x
    where
    toLevel :: String -> A.HeadingLevel
    toLevel "1" = A.H1
    toLevel "2" = A.H2
    toLevel "3" = A.H3
    toLevel "4" = A.H4
    toLevel "5" = A.H5
    toLevel "6" = A.H6

semanticCol :: (Int, Int) -> A.Width -> A.Position -> T.Body -> A.Col
semanticCol (gl, gr) l p (T.Col xs) = A.Col (semanticEach <$> xs) (quot 600 l) gl gr p

gutter :: Int
gutter = 20

semanticStyleRule :: [StyleBlock] -> Stylesheet -> Stylesheet
semanticStyleRule [] s = s
semanticStyleRule ((ClassBlock c r):xs) s =
    semanticStyleRule xs (addStyles c (semanticStyleEach <$> r) s)
    where
    semanticStyleEach :: T.Style -> A.Style
    semanticStyleEach (T.FontColor x)       = A.FontColor x
    semanticStyleEach (T.FontSize x)        = A.FontSize (read x)
    semanticStyleEach (T.BackgroundColor x) = A.BackgroundColor x
    semanticStyleEach (T.Width x)           = A.Width x
    semanticStyleEach (T.Height x)          = A.Height x
