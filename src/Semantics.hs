module Semantics where

import AST as A
import Tokens as T


semantic :: [T.Token] -> [A.AST]
semantic xs = semanticEach <$> xs

semanticEach :: T.Token -> A.AST
semanticEach (T.Text x)   = A.Text x
semanticEach (T.Img  s a) = A.Img  s a
semanticEach (T.Div  xs)  = A.Div  (semantic xs)
semanticEach (T.Col  xs)  = A.Col  (semantic xs)
semanticEach (T.Row  xs)  = A.Row  (semantic xs)
