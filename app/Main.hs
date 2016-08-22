module Main where

import System.Environment (getArgs)

import Data.Either.Combinators (fromRight')
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

import Parse (iParse, doc)
import Semantics (semantic)
import Compile (compile)


main :: IO ()
main = do
   [f]   <- getArgs
   s     <- readFile f
   case iParse doc f s of
        Left  err    -> print err
        Right result -> (putStr . renderHtml . compile . semantic) result
