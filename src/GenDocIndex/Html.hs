{-# Language PartialTypeSignatures #-}

module GenDocIndex.Html where

import GenDocIndex.Parse (PairMap, Key, Value(..), withValue, forceSingleVal)

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as M
import           Data.HashMap.Strict (HashMap,(!))
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 hiding (main, head, (!), ins, p)
import           Text.Blaze.Html5.Attributes hiding (title, id)

import Data.Function
import Data.List
import Data.Maybe
import Text.Blaze.Html.Renderer.Pretty

type NameMap = HashMap String PairMap

lookupError :: (Eq k, Hashable k) => String -> k -> HashMap k v -> v
lookupError e k = fromMaybe (error e) . M.lookup k

-- Sorts into GHC, GHC libs, and the rest
sortPairMaps :: [PairMap] -> (PairMap, NameMap, NameMap)
sortPairMaps = (\(g, l, r) -> (fromJust g, l, r)) 
    . foldl' sortMap (Nothing, M.empty, M.empty)
  where
    sortMap (g,l,r) m
        | n == "ghc"    = (Just m, l    , r    )
        | checkGhcLibs  = (g     , ins l, r    )
        | otherwise     = (g     , l    , ins r)
      where
        ins = M.insert n m
        n   = forceSingleVal $ head $ lookupError "Could not find name" "name" m
        checkGhcLibs = case M.lookup "haddock-html" m of
            Nothing -> False
            Just [v] -> isInfixOf "html/libraries" $ case v of
                SingleVal s  -> s
                BlockVal [s] -> s

makeDoc :: (PairMap, NameMap, NameMap) -> String
makeDoc (g,l,r) = renderHtml $ docTypeHtml $ do
    H.head $ title mainTitle
    body $ do
        h2 mainTitle
        dl $ do
            makeRow $ M.insert "haddock-html" [SingleVal ghcIndex] g
            foldMap makeRow $ elemsSortedByKey l
        hr
        dl $ foldMap makeRow $ elemsSortedByKey r
  where
    elemsSortedByKey :: Ord a => HashMap a b -> [b]
    elemsSortedByKey = fmap snd . sortBy (compare `on` fst) . M.toList

    mainTitle :: Markup
    mainTitle = string "Haskell Documentation"

    ghcIndex :: String
    ghcIndex  = 
        "/usr/share/doc/ghc-" ++ (forceSingleVal $ head $ lookupError "Could not find version" "version" g ) ++ "/html"

    makeRow :: PairMap -> Markup
    makeRow m = do
        dt $ maybe (string n) (makeLink n) d <> string ("(" ++ v ++ ")")
        dd $ maybe mempty string s <> maybe mempty homepage h
      where
        makeLink :: String -> String -> Markup
        makeLink t x = (a H.! href (stringValue x)) (string t)

        homepage :: String -> Markup
        homepage h' = string "(" <> makeLink "Homepage" h' <> string ")"

        pkgAttr :: Key -> Maybe String
        pkgAttr x = intercalate "\n" . fmap (withValue id (intercalate "\n")) <$> M.lookup x m

        n :: String
        n = fromJust $ pkgAttr "name"

        d :: Maybe String
        d = (++ "/index.html") <$> pkgAttr "haddock-html"

        s :: Maybe String
        s = pkgAttr "synopsis"

        h :: Maybe String
        h = pkgAttr "homepage"

        v :: String
        v = fromJust $ pkgAttr "version"
