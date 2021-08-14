{-# Language ApplicativeDo #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}

module GenDocIndex.Parse
    ( Key
    , Value(..)
    , PairMap
    , Parser
    , parser )
    where

import qualified Data.HashMap.Strict as M
import           Data.HashMap.Strict (HashMap)

import Control.Applicative (liftA2)
import Data.List
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

data Value =
      SingleVal String
    | BlockVal  [String]
    deriving (Show, Eq)

type Key = String
type PairMap = HashMap Key [Value]

type Parser = Parsec Void Text

parser :: Parser [PairMap]
parser = fmap (fmap mkPairMap) $ sepBy packageChunk sep <* end
  where
    sep = string "---" *> eol
    end = eol *> eof

mkPairMap :: [(Key, Value)] -> PairMap
mkPairMap = foldl' (\m (k,v) -> M.insertWith (++) k [v] m) M.empty

mkBlockVal :: [[String]] -> Value
mkBlockVal = BlockVal . fmap unwords

packageChunk :: Parser [(Key, Value)]
packageChunk = do
    h  <- alphaNumChar
    k  <- some $ satisfy (/= ':')
    _  <- char ':'
    v  <- choice
            [ SingleVal <$> singleVal
            , eol *> (mkBlockVal <$> blockVal) ]
    b  <- option [] packageChunk
    pure $ (h:k,v) : b

singleVal :: Parser String
singleVal = some (char ' ') *> someTill anySingle eol

blockVal :: Parser [[String]]
blockVal = choice
    [ eol *> blockVal
    , liftA2 (:) block blockVal
    , [] <$ lookAhead alphaNumChar  ]

block :: Parser [String]
block = some val
  where
    val :: Parser String
    val = some (char ' ') *> someTill anySingle eol
