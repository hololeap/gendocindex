
module GenDocIndex.Html where

import GenDocIndex.Parse (PairMap)

import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.HashMap.Lazy as HM
import           Data.HashMap.Lazy ((!))
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 hiding (main, head, (!), ins, p)
import           Text.Blaze.Html5.Attributes hiding (title, id)

import Data.List
import Data.Maybe
import Text.Blaze.Html.Renderer.Pretty

type NameMap = Map String PairMap

-- Sorts into GHC, GHC libs, and the rest
sortPairMaps :: [PairMap] -> (PairMap, NameMap, NameMap)
sortPairMaps = (\(g, l, r) -> (fromJust g, l, r)) 
    . foldl' sortMap (Nothing, M.empty, M.empty)
  where
    sortMap (g,l,r) m
        | n == "ghc"                     = (Just m, l    , r    )
        | (Just [p]) <- HM.lookup "haddock-html" m,
            isInfixOf "html/libraries" p = (g     , ins l, r    )
        | otherwise                      = (g     , l    , ins r)
      where
        ins = M.insert n m
        n   = head $ m ! "name"

makeDoc :: (PairMap, NameMap, NameMap) -> String
makeDoc (g,l,r) = renderHtml $ docTypeHtml $ do
    H.head $ title mainTitle
    body $ do
        h2 mainTitle
        dl $ do
            makeRow $ HM.insert "haddock-html" [ghcIndex] g
            foldMap makeRow $ M.elems l
        hr
        dl $ foldMap makeRow $ M.elems r
  where
    mainTitle = string "Haskell Documentation"
    ghcIndex  = 
        "/usr/share/doc/ghc-" ++ (head $ g ! "version") ++ "/html"
    makeRow m = do
        dt $ maybe (string n) (makeLink n) d <> string ("(" ++ v ++ ")")
        dd $ maybe mempty string s <> maybe mempty homepage h
      where
        makeLink t x = (a H.! href (stringValue x)) (string t)
        homepage h' = string "(" <> makeLink "Homepage" h' <> string ")"
        pkgAttr x = intercalate "\n" <$> HM.lookup x m
        n = fromJust $ pkgAttr "name"
        d = (++ "/index.html") <$> pkgAttr "haddock-html"
        s = pkgAttr "synopsis"
        h = pkgAttr "homepage"
        v = fromJust $ pkgAttr "version"
