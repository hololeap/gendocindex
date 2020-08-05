
module Main where

import GenDocIndex.Parse
import GenDocIndex.Html

import qualified Data.Text as T
import System.Process
import Text.Megaparsec
import UnliftIO.Exception

main = do
    dump <- readProcess "/usr/bin/ghc-pkg" ["dump"] ""
    let pms = parse parser "ghc-pkg dump" $ T.pack dump
    either (throwString . show) putStr $ makeDoc . sortPairMaps <$> pms
