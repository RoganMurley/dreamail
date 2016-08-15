module Main where

import System.Environment (getArgs)

import Data.Either.Combinators (fromRight')
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

import Parse (iParse, whole)
import Semantics (semantic)
import Translate (translate)


main :: IO ()
main = do
   [f]   <- getArgs
   s     <- readFile f
   case iParse whole f s of
        Left  err    -> print err
        Right result -> (putStr . renderHtml . translate . semantic) result
