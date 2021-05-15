{-# Language ApplicativeDo #-}

module GenDocIndex.Parse where

import qualified Data.HashMap.Strict as M
import           Data.HashMap.Strict (HashMap)

import Data.List
import Text.Parsec
import Text.Parsec.Text

type PairMap = HashMap String [String]

parser :: Parser [PairMap]
parser = fmap (fmap finalize) $ sepBy block sep <* end
  where
    block :: Parser [(String, String)]
    block = do
        h  <- alphaNum
        k  <- manyTill anyChar (char ':')
        vs <- try vals <|> (endOfLine *> vals)
        b  <- option [] block
        pure $ (f h k <$> vs) ++ b
      where
        vals :: Parser [String]
        vals = do
            _  <- many1 (oneOf "\t ")
            v  <- manyTill anyChar endOfLine
            vs <- option [] vals
            pure $ v : vs

        f h k v = (h:k,v)

    sep = string "---\n"
    end = endOfLine *> eof

    finalize :: [(String, String)] -> PairMap
    finalize = foldl' addToPairMap M.empty
      where
        addToPairMap m (k,v) = M.insertWith (++) k [v] m
        
