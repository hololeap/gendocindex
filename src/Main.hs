
module Main where

import GenDocIndex.Error
import GenDocIndex.Parse
import GenDocIndex.Html

import qualified Data.Text as T
import System.Process
import Text.Megaparsec
import UnliftIO.Exception

main :: IO ()
main = do
    dump <- readProcess "/usr/bin/ghc-pkg" ["dump"] ""
    pms <- fromEither $ parse parser "ghc-pkg dump" $ T.pack dump
    s <- fromEither (splitPairMaps pms >>= toGDE . makeDoc)
    putStrLn s
