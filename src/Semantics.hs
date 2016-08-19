module Semantics where

import AST as A
import Tokens as T


semantic :: [T.Token] -> A.Root
semantic xs = A.Root (semanticRow <$> xs)

semanticRow :: T.Token -> A.Row
semanticRow (T.Row xs) = A.Row $ fmlMap
    (semanticCol (gutter, quot gutter 2) (length xs) First)
    (semanticCol (quot gutter 2, quot gutter 2) (length xs) Middle)
    (semanticCol (quot gutter 2, gutter) (length xs) Last)
    xs

semanticEach :: T.Token -> A.AST
semanticEach (T.Text x)      = A.Text x
semanticEach (T.Img  s a)    = A.Img s a
semanticEach (T.Div  xs)     = A.Div (semanticEach <$> xs)
semanticEach (T.A u xs)      = A.A u (semanticEach <$> xs)
semanticEach (T.Heading l x) = A.Heading (toLevel l) x
    where
    toLevel :: String -> A.HeadingLevel
    toLevel "1" = A.H1
    toLevel "2" = A.H2
    toLevel "3" = A.H3
    toLevel "4" = A.H4
    toLevel "5" = A.H5
    toLevel "6" = A.H6

semanticCol :: (Int, Int) -> A.Width -> A.Position -> T.Token -> A.Col
semanticCol (gl, gr) l p (T.Col xs) = A.Col (semanticEach <$> xs) (quot 600 l) gl gr p

gutter :: Int
gutter = 20

-- Map with different functions for first, middle and last elements.
fmlMap :: (a -> b) -> (a -> b) -> (a -> b) -> [a] -> [b]
fmlMap f m l [] = []
fmlMap f m l [a] = [f a]
fmlMap f m l [a, b] = [f a, l b]
fmlMap f m l xs = [f (head xs)] ++ (m <$> (init (tail xs))) ++ [l (last xs)]
