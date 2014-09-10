{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Hbc.Scraper (
                     CsvData
                   , GetData
                   , Spreadsheets
                   , getSpreadsheets
                   , getURL
                   , decode
                   , exceptT
                   , ResultColumn(..)
                   , getOutRows
                   , escape
                   ) where

import Data.Char (ord)
import Data.Monoid ((<>), mempty)
import Data.Word (Word8)
import Control.Monad.Except

import Data.Vector (Vector)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)
import Blaze.ByteString.Builder (fromByteString, fromWord8, toLazyByteString)
import Data.Csv (decodeWith, HasHeader(..), DecodeOptions(..),
                 defaultDecodeOptions)


import Network.HTTP
import Network.URI (parseURI)

type CsvData = Vector (Vector ByteString)

type GetData a = ExceptT String IO a

type Spreadsheets a = Map a CsvData

getSpreadsheets :: (Ord a) => [(a, String)] -> GetData (Spreadsheets a)
getSpreadsheets urls = foldM addSpreadsheet M.empty urls
  where
      addSpreadsheet acc (table, url) = do
          response <- getURL url
          csv <- decode response
          return $ M.insert table csv acc

getURL :: String -> GetData ByteString
getURL url = case parseURI url of
                 Nothing  -> throwError $ "Invalid URL: " ++ url
                 Just uri -> do
                     resp <- liftIO . simpleHTTP $ defaultGETRequest_ uri
                     liftIO $ getResponseBody resp

decode :: ByteString -> GetData CsvData
decode = exceptT . decodeCommaSeparated
  where
      decodeCommaSeparated = decodeWith opts NoHeader
      opts = defaultDecodeOptions { decDelimiter = comma }
      comma = fromIntegral (ord ',')

exceptT :: MonadError a m => Either a a1 -> m a1
exceptT = either throwError return

data ResultColumn = ResultColumn {
                                   rCode        :: !ByteString
                                 , rDescription :: !ByteString
                                 , rValue       :: !ByteString
                                 } deriving (Eq, Show)

-- ByteString is the date
getOutRows :: ByteString -> [ResultColumn] -> [ByteString]
getOutRows date rs = [codes, descs, vals]
  where
      codes = enc $ ""     : fmap (esc . rCode) rs
      descs = enc $ "Date" : fmap (esc . rDescription) rs
      vals  = enc $ date   : fmap (esc . rValue) rs
      enc = BL.intercalate ","
      esc = escape (fromIntegral (ord ','))


-- yanked from cassava
escape :: Word8 -> ByteString -> ByteString
escape !delim !s
    | BL.any (\ b -> b == dquote || b == delim || b == nl || b == cr || b == sp)
        s = toLazyByteString $
            fromWord8 dquote
            <> BL.foldl
                (\ acc b -> acc <> if b == dquote
                    then fromByteString "\"\""
                    else fromWord8 b)
                mempty
                s
            <> fromWord8 dquote
    | otherwise = s
  where
    dquote = 34
    nl     = 10
    cr     = 13
    sp     = 32

