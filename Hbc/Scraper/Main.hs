module Main (main) where

-- import qualified Hbc.Scraper.Test as Test

import Network.Wreq
import Control.Lens
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Encoding.Error as T

import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (decimal)

import Data.Csv (decodeWith, HasHeader(..), DecodeOptions(..), defaultDecodeOptions)
import qualified Data.ByteString.Lazy as BL

import Data.Char (ord)
import Control.Applicative

import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

import Control.Monad (foldM)

type CsvData = V.Vector (V.Vector BL.ByteString)

main :: IO ()
main = do
        r <- get "http://ir.eia.gov/wpsr/table1.csv"
        let t = r ^? responseHeader "Content-Type"
        if t == Just "text/plain" then do
            -- print $ r ^. responseBody
            let c = getCsvData $ r ^. responseBody
            -- let c = r ^. responseBody & T.decodeUtf8With T.lenientDecode
            -- let c = r ^. responseBody & T.decodeUtf8With (T.replace 'Q')
            -- let c = r ^. responseBody & T.decodeUtf16BE
            print c
            else
                return ()
  where
    getCsvData :: BL.ByteString -> Either String CsvData
    getCsvData = decodeWith opts NoHeader
    opts = defaultDecodeOptions { decDelimiter = fromIntegral (ord ',') }

data WeeklySpreadsheet = Table1
                       | Table1Part2
                       | Table2
                       | Table3
                       | Table7
                       | Table9 deriving (Eq, Ord, Show, Read)

urls :: [(WeeklySpreadsheet, String)]
urls = -- [ (Table1, "http://ir.eia.gov/wpsr/table1.csv")
       -- , (Table1Part2, "http://ir.eia.gov/wpsr/table1.csv")
       -- , (Table2, "http://ir.eia.gov/wpsr/table2.csv")
       -- , (Table3, "http://ir.eia.gov/wpsr/table3.csv")
       -- , (Table7, "http://ir.eia.gov/wpsr/table7.csv")
       [ (Table9, "http://ir.eia.gov/wpsr/table9.csv") ]

getData :: IO (Either String (Map WeeklySpreadsheet CsvData))
getData = foldM getData (Right M.empty) urls
  where
    -- getData :: Map WeeklySpreadsheet CsvData -> (WeeklySpreadsheet, String)
    getData e@(Left _) _ = return e
    getData (Right m) (sheet, url) = do
        response <- get url
        let body = getCsvData $ response ^. responseBody in
            case body of
                Left e -> return $ Left e
                Right csvData -> return . Right $ M.insert sheet csvData m
    getCsvData :: BL.ByteString -> Either String CsvData
    getCsvData = decodeWith opts NoHeader
    opts = defaultDecodeOptions { decDelimiter = fromIntegral (ord ',') }

decodeCommaSeparated :: BL.ByteString -> Either String CsvData
decodeCommaSeparated = decodeWith opts NoHeader
  where
    opts :: DecodeOptions
    opts = defaultDecodeOptions { decDelimiter = fromIntegral (ord ',') }

fetchUrl url = do
        response <- get url
        let body = response ^. responseBody
        return body


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

parseSimpleDate' :: Parser SimpleDate
parseSimpleDate' = do
        month <- decimal
        sep
        day <- decimal
        sep
        year <- decimal
        return $ Date month day year
  where
    sep = word8 . fromIntegral $ ord '/'

