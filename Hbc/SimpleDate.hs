module Hbc.SimpleDate (
                        SimpleDate(..)
                      , parseSimpleDate
                      ) where

import Data.Char (ord)
import Control.Applicative
import Data.Attoparsec.ByteString.Lazy
import Data.Attoparsec.ByteString.Char8 (decimal)

data SimpleDate = Date { sMonth :: !Int
                       , sDay   :: !Int
                       , sYear  :: !Int
                       } deriving (Eq, Show, Read)

instance Ord SimpleDate where
        compare (Date m d y) (Date m' d' y') = compare (y, m, d) (y', m', d')

parseSimpleDate :: Parser SimpleDate
parseSimpleDate = Date <$> decimal <* sep <*> decimal <* sep <*> decimal
  where
    sep = word8 . fromIntegral $ ord '/'

