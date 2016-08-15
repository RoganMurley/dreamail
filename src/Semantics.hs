module Semantics where

import AST as A
import Tokens as T


semantic :: [T.Token] -> [A.AST]
semantic xs = semanticEach <$> xs

semanticEach :: T.Token -> A.AST
semanticEach (T.Text x)   = A.Text x
semanticEach (T.Img  s a) = A.Img  s a
semanticEach (T.Div  xs)  = A.Div  (semantic xs)
semanticEach (T.Row  xs)  = A.Row  $ fmlMap
    (semanticColEach (gutter, quot gutter 2) (length xs) First)
    (semanticColEach (quot gutter 2, quot gutter 2) (length xs) Middle)
    (semanticColEach (quot gutter 2, gutter) (length xs) Last)
        xs

semanticColEach :: (Int, Int) -> A.Width -> A.Position -> T.Token -> A.AST
semanticColEach (gl, gr) l p (T.Col xs) = A.Col (semantic xs) (quot 600 l) gl gr p

gutter :: Int
gutter = 20

-- Map with different functions for first, middle and last elements.
fmlMap :: (a -> b) -> (a -> b) -> (a -> b) -> [a] -> [b]
fmlMap f m l [] = []
fmlMap f m l [a] = [f a]
fmlMap f m l [a, b] = [f a, l b]
fmlMap f m l xs = [f (head xs)] ++ (m <$> (init (tail xs))) ++ [l (last xs)]
