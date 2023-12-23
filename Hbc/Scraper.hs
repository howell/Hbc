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
                   , readDataPoints
                   , writeDataPoints
                   ) where

import Data.Char (ord)
import Data.Monoid ((<>), mempty)
import Data.Word (Word8)
import Control.Monad.Except

import Control.Concurrent.Async

import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)
import Blaze.ByteString.Builder (fromByteString, fromWord8, toLazyByteString)
import Data.Csv (decodeWith, HasHeader(..), DecodeOptions(..),
                 defaultDecodeOptions)


import Network.Wreq (get, responseBody)
import Control.Lens ((^.))

type CsvData = Vector (Vector ByteString)

type GetData a = ExceptT String IO a

type Spreadsheets a = Map a CsvData

{-
getSpreadsheets :: (Ord a) => [(a, String)] -> GetData (Spreadsheets a)
getSpreadsheets points = do
        mvars <- liftIO . sequence $ repeat newEmptyMVar
        let points' = zip points mvars
        forM_ points' (liftIO . forkIO . fmap (const ()) . runExceptT . getCsv)
        foldM addEntry M.empty mvars
  where
      getCsv ((key, url), mvar) = do
            !response <- getURL url
            !csv <- decode response
            liftIO $ putMVar mvar (key, csv)
      addEntry acc mvar = do
          !(!key, !csv) <- liftIO $ takeMVar mvar
          return $ M.insert key csv acc
-}

getSpreadsheets :: (Ord a) => [(a, String)] -> GetData (Spreadsheets a)
getSpreadsheets points | points `seq` False = undefined
getSpreadsheets points = do
        !asyncs <- liftIO . sequence . fmap fetch $ points
        foldM f M.empty asyncs
  where
      fetch !(!k, !url) = fmap ((,) k) $ async (getURL' url)
      f !acc !(!k, !act) = do
          !response <- liftIO $ wait act
          !csv <- decode response
          return $ M.insert k csv acc


getURL' :: String -> IO ByteString
getURL' !url = do
        r <- get url
        return $ r ^. responseBody

{-
getSpreadsheets :: (Ord a) => [(a, String)] -> GetData (Spreadsheets a)
getSpreadsheets urls = foldM addSpreadsheet M.empty urls
  where
      addSpreadsheet acc (table, url) = do
          !response <- getURL url
          !csv <- decode response
          return $ M.insert table csv acc
-}

getURL :: String -> GetData ByteString
getURL !url = do
        r <- liftIO $ get url
        return $ r ^. responseBody

decode :: ByteString -> GetData CsvData
decode = exceptT . decodeCommaSeparated
  where
      decodeCommaSeparated = decodeWith opts NoHeader
      !opts = defaultDecodeOptions { decDelimiter = comma }
      !comma = fromIntegral (ord ',')

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


readDataPoints :: FilePath -> (Vector ByteString -> Maybe a) -> GetData [a]
readDataPoints fp convert = do
       raw <- liftIO $ BL.readFile fp
       csv <- decode raw
       let rows = V.tail csv
       dataPoints <- maybeExceptT "Failed to parse data" (V.mapM convert rows)
       return $ V.toList dataPoints

maybeExceptT :: MonadError a m => a -> Maybe a1 -> m a1
maybeExceptT msg = maybe (throwError msg) return

writeDataPoints :: FilePath -> Vector ByteString -> [a] -> (a -> Vector ByteString) -> IO ()
writeDataPoints fp headers dps convert = BL.writeFile fp csv
  where
    csv = formatCsv $ dataPointsToCsv headers dps convert

dataPointsToCsv :: Vector ByteString -> [a] -> (a -> Vector ByteString) -> Vector (Vector ByteString)
dataPointsToCsv headers dps convert = V.fromList $ headers : map convert dps

formatCsv :: Vector (Vector ByteString) -> ByteString
formatCsv rows = BL.intercalate "\n" rows'
  where
    rows' = encodeRow <$> (V.toList rows)
    encodeRow r = BL.intercalate "," $ esc <$> V.toList r
    esc = escape (fromIntegral (ord ','))
