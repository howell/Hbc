{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Hbc.Weekly (
                   runBot
                  ) where

import Hbc.Scraper
import Hbc.SimpleDate

import Control.Applicative
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Char (ord)
import Control.Monad.Except

import qualified Data.Vector as V
import Data.Vector (Vector, (!), (!?))
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)
import Data.Attoparsec.ByteString.Lazy
import Data.Attoparsec.ByteString.Char8 (decimal)

import Text.Read (readMaybe)

data WeeklySpreadsheet = Table1
                       | Table2
                       | Table3
                       | Table7
                       | Table9 deriving (Eq, Ord, Show, Read)

data DataPoint = DataPoint { sheet       :: !WeeklySpreadsheet,
                             rowI        :: !Int,
                             description :: !ByteString,
                             code        :: !ByteString
                           } deriving (Eq, Show)

weeklyURLs :: [(WeeklySpreadsheet, String)]
weeklyURLs = [
               (Table1, "http://ir.eia.gov/wpsr/table1.csv")
             , (Table2, "http://ir.eia.gov/wpsr/table2.csv")
             , (Table3, "http://ir.eia.gov/wpsr/table3.csv")
             , (Table7, "http://ir.eia.gov/wpsr/table7.csv")
             , (Table9, "http://ir.eia.gov/wpsr/table9.csv")
             ]

type LatestColumns = Map WeeklySpreadsheet (Vector ByteString)

getWeeklys :: GetData LatestColumns
getWeeklys = M.mapWithKey getLatestColumn <$> getSpreadsheets weeklyURLs
  where
      getLatestColumn :: WeeklySpreadsheet -> CsvData -> Vector ByteString
      getLatestColumn Table1 !csv = let (a, b) = V.splitAt 20 csv in
                                        latestC a <> latestC b
      getLatestColumn _      !csv = latestC csv
      latestC !csv = fmap (fromMaybe BL.empty . (!? latestI)) csv
        where
            !latestI = V.maxIndexBy (comparing parseDate) (csv ! 0)
            !parseDate = maybeResult . parse parseSimpleDate

getDataPoint :: LatestColumns -> DataPoint -> Maybe ByteString
getDataPoint !cs !(DataPoint { sheet = s, rowI = i }) = do
        !column <- M.lookup s cs
        column !? i

getADate :: LatestColumns -> Maybe ByteString
getADate !cs = getDate Table1 <|> getDate Table2 <|> getDate Table3 <|>
              getDate Table7 <|> getDate Table9
  where
      getDate t = M.lookup t cs >>= (!? 0)

getResult :: LatestColumns -> DataPoint -> Either String ResultColumn
getResult !cs !d@(DataPoint { description = desc, code = c }) =
        maybe err ok $ getDataPoint cs d
  where
      !err = Left $ "Couldn't find data point: " ++ show d
      !ok  = Right . ResultColumn c desc

getResults :: LatestColumns -> [DataPoint] -> Either String [ResultColumn]
getResults !cols = sequence . fmap (getResult cols)

runBot :: FilePath -> GetData ()
runBot fpath = do
        !dataPoints <- readDataPoints "weekly_data.csv" readDataPoint
        !r <- getWeeklys
        !date <- maybe (throwError "Could not find date") return $ getADate r
        !result <- exceptT $ getResults' dataPoints r
        let !out = BL.intercalate "\n" $ getOutRows date result
        liftIO $ BL.writeFile fpath out
  where
      getResults' = flip getResults

headers :: Vector ByteString
headers = V.fromList ["Description", "Label", "Table #", "Row"]

writeWeeklyDataPoints :: FilePath -> [DataPoint] -> IO ()
writeWeeklyDataPoints fp dps = writeDataPoints fp headers dps writeDataPoint

writeDataPoint :: DataPoint -> Vector ByteString
writeDataPoint (DataPoint tbl rowI0 desc label) =
  V.fromList $ [desc, label, table, rowI]
  where
    table = fromString [last $ show tbl]
    rowI  = fromString $ show $ rowI0 + 1

readDataPoint :: Vector ByteString -> Maybe DataPoint
readDataPoint csv = do
  desc <- csv !? 0
  label <- csv !? 1
  tableNoRaw <- csv !? 2
  tableNo <- maybeResult $ parse decimal tableNoRaw :: Maybe Int
  table <- (readMaybe $ "Table" ++ (show tableNo)) :: Maybe WeeklySpreadsheet
  rowNoRaw <- csv !? 3
  rowNo <- maybeResult $ parse decimal rowNoRaw
  let rowNoZeroBased = rowNo - 1
  return $ DataPoint table rowNoZeroBased desc label

