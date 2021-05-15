{-# Language DeriveAnyClass #-}
{-# Language ExistentialQuantification #-}
{-# Language FlexibleContexts #-}

module GenDocIndex.Error
    ( PkgAttr
    , SomeGenDocException(..)
    , toGDE
    , GHCMissingError(..)
    , AttrMissingError(..)
    , lookupAttr
    ) where

import Control.Monad.Except
import Data.Bifunctor (first)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Typeable
import UnliftIO.Exception

type PkgAttr = String

data SomeGenDocException = forall e. Exception e => SomeGenDocException e

instance Show SomeGenDocException where
    show (SomeGenDocException e) = show e

instance Exception SomeGenDocException where
    displayException (SomeGenDocException e) = displayException e

toGDE :: ( MonadError SomeGenDocException m, Exception e )
    => Either e a -> m a
toGDE = liftEither . first SomeGenDocException

gdeToException :: Exception e => e -> SomeException
gdeToException = toException . SomeGenDocException

gdeFromException :: Exception e => SomeException -> Maybe e
gdeFromException x = do
    SomeGenDocException a <- fromException x
    cast a

data GHCMissingError = GHCMissingError
    deriving Show

instance Exception GHCMissingError where
    toException = gdeToException
    fromException = gdeFromException
    displayException _ =
        "GHCMissingError: GHC is missing from packge database"

newtype AttrMissingError = AttrMissingError PkgAttr
    deriving Show

instance Exception AttrMissingError where
    toException = gdeToException
    fromException = gdeFromException

lookupAttr :: MonadError AttrMissingError m
    => PkgAttr -> HashMap PkgAttr v -> m v
lookupAttr k m = case M.lookup k m of
    Just v  -> pure v
    Nothing -> throwError (AttrMissingError k)
