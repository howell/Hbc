{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Hbc.Monthly where

import Hbc.Scraper

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Vector ((!?), Vector)
import qualified Data.Vector as V
import Control.Monad.Except
import Data.Char (ord, chr, isDigit, toUpper, toLower)
import Data.Attoparsec.ByteString.Lazy
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.String (fromString)
import Text.Read (readMaybe)

data MonthlySpreadsheet = MonthlyTable1
                        | MonthlyTable5
                        | MonthlyTable9
                        | MonthlyTable13
                        | MonthlyTable17
                        | MonthlyTable21 deriving (Eq, Ord, Show, Read)

data DataPoint = DataPoint { sheet       :: !MonthlySpreadsheet,
                             rowI        :: !Int,
                             colI        :: !Int,
                             description :: !ByteString,
                             code        :: !ByteString
                           } deriving (Eq, Show)

type MonthlySpreadsheets = Map MonthlySpreadsheet CsvData

monthlyURLs :: [(MonthlySpreadsheet, String)]
monthlyURLs = [
                (MonthlyTable1, "http://www.eia.gov/petroleum/supply/monthly/csv/table1.csv")
              , (MonthlyTable5, "http://www.eia.gov/petroleum/supply/monthly/csv/table5.csv")
              , (MonthlyTable9, "http://www.eia.gov/petroleum/supply/monthly/csv/table9.csv")
              , (MonthlyTable13, "http://www.eia.gov/petroleum/supply/monthly/csv/table13.csv")
              , (MonthlyTable17, "http://www.eia.gov/petroleum/supply/monthly/csv/table17.csv")
              , (MonthlyTable21, "http://www.eia.gov/petroleum/supply/monthly/csv/table21.csv")
              ]

getDataPoint :: MonthlySpreadsheets -> DataPoint -> Maybe ByteString
getDataPoint sheets (DataPoint { sheet = s, rowI = r, colI = col }) = do
        csv <- M.lookup s sheets
        row <- csv !? r
        row !? col

getResult :: MonthlySpreadsheets -> DataPoint -> Either String ResultColumn
getResult sheets d@(DataPoint { description = desc, code = c }) =
        maybe err ok $ getDataPoint sheets d
  where
      err = Left $ "Couldn't find data point: " ++ show d
      ok  = Right . ResultColumn c desc

getResults :: MonthlySpreadsheets -> [DataPoint] -> Either String [ResultColumn]
getResults cols = sequence . fmap (getResult cols)

runBot :: FilePath -> GetData ()
runBot fpath = do
        !dataPoints <- readDataPoints "monthly_data.csv" readDataPoint
        r <- getSpreadsheets monthlyURLs
        result <- exceptT $ getResults' dataPoints r
        let date = ""
            out = BL.intercalate "\n" $ getOutRows date result
        liftIO $ BL.writeFile fpath out
  where
      getResults' = flip getResults

headers :: Vector ByteString
headers = V.fromList ["Description", "Label", "Table #", "Row", "Column"]

columnToIndex :: String -> Int
columnToIndex c = columnToIndex' (map toLower c) 0
  where
    columnToIndex' [] acc = acc
    columnToIndex' (c:cs) acc = columnToIndex' cs (charIndex c + acc * 26)
    charIndex c = ord c - ord 'a'

indexToColumn :: Int -> String
indexToColumn i | i < 26 = [convertDigit i]
indexToColumn i = let (quot, rem) = i `divMod` 26
                  in indexToColumn quot ++ [convertDigit i]

convertDigit :: Int -> Char
convertDigit i = toUpper . chr $ ord 'a' + i

writeMonthlyDataPoints :: FilePath -> [DataPoint] -> IO ()
writeMonthlyDataPoints fp dps = writeDataPoints fp headers dps writeDataPoint

writeDataPoint :: DataPoint -> Vector ByteString
writeDataPoint (DataPoint tbl rowI0 colI0 desc label) =
  V.fromList $ [desc, label, table, rowI, col]
  where
    table = fromString $ dropWhile (not . isDigit) $ show tbl
    rowI  = fromString $ show $ rowI0 + 1
    col   = fromString $ indexToColumn colI0

readDataPoint :: Vector ByteString -> Maybe DataPoint
readDataPoint csv = do
  desc <- csv !? 0
  label <- csv !? 1
  tableNoRaw <- csv !? 2
  tableNo <- maybeResult $ parse decimal tableNoRaw :: Maybe Int
  table <- (readMaybe $ "MonthlyTable" ++ (show tableNo)) :: Maybe MonthlySpreadsheet
  rowNoRaw <- csv !? 3
  rowNo <- maybeResult $ parse decimal rowNoRaw
  let rowNoZeroBased = rowNo - 1
  colRaw <- csv !? 4
  let col = columnToIndex (unpack colRaw)
  return $ DataPoint table rowNoZeroBased col desc label

