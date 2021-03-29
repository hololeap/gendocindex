{-# Language FlexibleContexts #-}
{-# Language LambdaCase #-}
{-# Language MultiWayIf #-}
{-# Language PartialTypeSignatures #-}
{-# Language ScopedTypeVariables #-}

module GenDocIndex.Html 
    ( makeDoc
    , splitPairMaps
    ) where

import GenDocIndex.Parse (PairMap, Key, Value(..))
import GenDocIndex.Error

import qualified Data.HashMap.Strict as M
import           Data.HashMap.Strict (HashMap)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 hiding (main, head, (!), ins, p)
import           Text.Blaze.Html5.Attributes hiding (title, id)

import Control.Monad.Except
import Data.Foldable (fold)
import Data.Function (on)
import Data.List (intercalate, isInfixOf, sortBy)
import Data.Semigroup (First(..))
import Text.Blaze.Html.Renderer.Pretty

type NameMap = HashMap String PairMap

makeDoc :: forall m. MonadError AttrMissingError m =>
    (PairMap, NameMap, NameMap) -> m String
makeDoc (g0,l0,r0) = do
    ghcIndex <- getGHCIndex
    g <- makeRow (M.insert "haddock-html" [SingleVal ghcIndex] g0)
    l <- fold <$> traverse makeRow (elemsSortedByKey l0)
    r <- fold <$> traverse makeRow (elemsSortedByKey r0)
    pure $ renderHtml $ docTypeHtml $ do
        H.head $ title mainTitle
        body $ h2 mainTitle <> dl (g <> l) <> hr <> dl r
  where
    elemsSortedByKey :: Ord a => HashMap a b -> [b]
    elemsSortedByKey = fmap snd . sortBy (compare `on` fst) . M.toList

    mainTitle :: Markup
    mainTitle = string "Haskell Documentation"

    getGHCIndex :: m String
    getGHCIndex  = do
        gv <- pkgAttrE "version" g0
        pure $ "/usr/share/doc/ghc-" <> gv <> "/html"

    makeRow :: PairMap -> m Markup
    makeRow m = do
        n <- pkgAttrE "name" m
        let d = (++ "/index.html") <$> pkgAttr "haddock-html" m
        v <- pkgAttrE "version" m
        let hackageLink = "https://hackage.haskell.org/package/" ++ n
        pure $ do
            dt $ do
                maybe (string n) (makeLink n) d
                string ("(" ++ v ++ ")")
            dd $ do
                pkgAttrMon string "synopsis" m
                pkgAttrMon homepage "homepage" m
                string "("
                makeLink "Hackage" hackageLink
                string ")"

-- Sorts into GHC, GHC libs, and the rest
splitPairMaps :: MonadError SomeGenDocException m =>
    [PairMap] -> m (PairMap, NameMap, NameMap)
splitPairMaps ps = do
    ts <- toGDE $ traverse sortMap ps
    let (gm, l, o) = fold ts
    g <- toGDE $ case gm of
        Just (First m) -> pure m
        Nothing        -> throwError GHCMissingError
    pure (g, l, o)
  where
    sortMap :: MonadError AttrMissingError m
        => PairMap -> m (Maybe (First PairMap), NameMap, NameMap)
    sortMap m = do
        n <- pkgAttrE "name" m
        let hm = pkgAttr "haddock-html" m
        pure $ case (n == "ghc", isInfixOf "html/libraries" <$> hm) of
            (True, _) ->
                (Just (First m), mempty         , mempty         )
            (_, Just True) ->
                (mempty        , M.singleton n m, mempty         )
            _ ->
                (mempty        , mempty         , M.singleton n m)

makeLink :: String -> String -> Markup
makeLink txt addr = (a H.! href (stringValue addr)) (string txt)

homepage :: String -> Markup
homepage addr = string "(" <> makeLink "Homepage" addr <> string ")"

pkgAttr :: Key -> PairMap -> Maybe String
pkgAttr k m = valuesToString <$> M.lookup k m

pkgAttrE :: MonadError AttrMissingError m
    => Key -> PairMap -> m String
pkgAttrE k m = valuesToString <$> lookupAttr k m

pkgAttrMon :: Monoid m => (String -> m) -> Key -> PairMap -> m
pkgAttrMon f k m = foldMap f $ pkgAttr k m

valuesToString :: [Value] -> String
valuesToString vs = intercalate "\n" $ vs >>= \case
    SingleVal s -> [s]
    BlockVal bl -> bl
