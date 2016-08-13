module Main where

import System.Environment (getArgs)

import Data.Either.Combinators (fromRight')
import Text.Parsec (parse)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

import Parse (whole)
import Translate (translate)


main :: IO ()
main = do
   [f]   <- getArgs
   s     <- readFile f
   (putStr . renderHtml . translate . fromRight') (parse whole f s)
