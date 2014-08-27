{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Hbc.Weekly (
                   runBot
                  ) where

import Hbc.Scraper
import Hbc.SimpleDate

import Data.Char (ord)
import Control.Applicative
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Control.Monad.Except

import qualified Data.Vector as V
import Data.Vector (Vector, (!), (!?))
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)
import Data.Attoparsec.ByteString.Lazy

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
      getLatestColumn Table1 csv = let (a, b) = V.splitAt 20 csv in
                                        latestC a <> latestC b
      getLatestColumn _      csv = latestC csv
      latestC csv = fmap (fromMaybe BL.empty . (!? latestI)) csv
        where
            latestI = V.maxIndexBy (comparing parseDate) (csv ! 0)
            parseDate = maybeResult . parse parseSimpleDate

getDataPoint :: LatestColumns -> DataPoint -> Maybe ByteString
getDataPoint cs (DataPoint { sheet = s, rowI = i }) = M.lookup s cs >>= (!? i)

getADate :: LatestColumns -> Maybe ByteString
getADate cs = getDate Table1 <|> getDate Table2 <|> getDate Table3 <|>
              getDate Table7 <|> getDate Table9
  where
      getDate t = M.lookup t cs >>= (!? 0)

getResult :: LatestColumns -> DataPoint -> Maybe ResultColumn
getResult cs d@(DataPoint { description = desc, code = c }) =
        fmap (ResultColumn c desc) (getDataPoint cs d)

getResults :: LatestColumns -> [DataPoint] -> Maybe [ResultColumn]
getResults cols = sequence . fmap (getResult cols)

-- ByteString is the date
getOutRows :: ByteString -> [ResultColumn] -> [ByteString]
getOutRows date rs = [codes, descs, vals]
  where
      codes = enc $ ""     : fmap (esc . rCode) rs
      descs = enc $ "Date" : fmap (esc . rDescription) rs
      vals  = enc $ date   : fmap (esc . rValue) rs
      enc = BL.intercalate ","
      esc = escape (fromIntegral (ord ','))

runBot :: FilePath -> GetData ()
runBot fpath = do
        r <- getWeeklys
        date <- maybe' "Could not find date" $ getADate r
        result <- maybe' "Could not find all columns" $ getResults' dataPoints r
        let out = BL.intercalate "\n" $ getOutRows date result
        liftIO $ BL.writeFile fpath out
  where
      maybe' e = maybe (throwError e) return
      getResults' = flip getResults

dataPoints :: [DataPoint]
dataPoints = concat [table1Data2, table1Data1, table1Data22, table2Data1,
                    table9Data3, table3Data1, table7Data1, table9Data6,
                    table2Data11, table9Data2]
  where
      table1Data2 = [ DataPoint Table1 34 "Weekly U.S. Crude Oil Inputs into Refineries  (Thousand Barrels per Day)" "WCRRIUS2"
                    , DataPoint Table1 37 "Weekly U.S. Production of Oxygenates and Renewable Fuels  (Thousand Barrels per Day)" "W_EPOOXR_YPT_NUS_MBBLD"
                    , DataPoint Table1 38 "Weekly U.S. Oxy Plant Production of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)" "W_EPOOXE_YOP_NUS_MBBLD"
                    , DataPoint Table1 21 "Weekly U.S. Crude Oil Field Production  (Thousand Barrels per Day)" "WCRFPUS2"
                    , DataPoint Table1 22 "Weekly Alaska Refinery Field Production of Crude Oil  (Thousand Barrels per Day)" "W_EPC0_FPF_SAK_MBBLD"
                    ]
      table1Data1 = [ DataPoint Table1 2 "Weekly U.S. Crude Oil Ending Stocks Excluding SPR  (Thousand Barrels)" "WCESTUS1"
                    , DataPoint Table1 4 "Weekly U.S. Total Gasoline Ending Stocks  (Thousand Barrels)" "WGTSTUS1"
                    , DataPoint Table1 8 "Weekly U.S. Ending Stocks of Oxygenates Fuel Ethanol  (Thousand Barrels)" "W_EPOOXE_SAE_NUS_MBBL"
                    , DataPoint Table1 10 "Weekly U.S. Total Distillate Ending Stocks  (Thousand Barrels)" "WDISTUS1"
                    ]
      table1Data22 = [ DataPoint Table1 47 "Weekly U.S. Finished Motor Gasoline Product Supplied  (Thousand Barrels per Day)" "WGFUPUS2"
                     , DataPoint Table1 49 "Weekly U.S. Total Distillate Fuel Oil Product Supplied  (Thousand Barrels per Day)" "WGFUPUS2"
                     ]
      table2Data1 = [
                      DataPoint Table2 19 "Weekly U.S. Percent Utilization of Refinery Operable Capacity  (Percent)" "WPULEUS3"
                    ]
      table9Data3 = [
                      DataPoint Table9 64 "Weekly U.S. Refinery and Blender Adjusted Net Production of Finished Motor Gasoline (Thousand Barrels per Day)" "WGFRPUS2"
                    , DataPoint Table9 72 "Weekly U.S. Refinery and Blender Net Production of Finished Reformulated Motor Gasoline   (Thousand Barrels per Day)" "WGRRPUS2"
                    , DataPoint Table9 78 "Weekly U.S. Refinery and Blender Net Production of Finished Reformulated Motor Gasoline with Ethanol   (Thousand Barrels per Day)" "WG1TP_NUS_2"
                    , DataPoint Table9 84 "Weekly U.S. Refinery and Blender Net Production of Other Finished Reformulated Motor Gasoline  (Thousand Barrels per Day)" "W_EPM0RO_YPT_NUS_MBBLD"
                    , DataPoint Table9 90 "Weekly U.S. Refinery and Blender Net Production of Finished Conventional Motor Gasoline  (Thousand Barrels per Day)" "WG4TP_NUS_2"
                    , DataPoint Table9 96 "Weekly U.S. Refinery and Blender Net Production of Finished Conventional Motor Gasoline with Ethanol  (Thousand Barrels per Day)" "WG5TP_NUS_2"
                    , DataPoint Table9 102 "Weekly U.S. Refinery and Blender Net Production of Finished Conventional Motor Gasoline, Ed 55 and Lower  (Thousand Barrels per Day)" "W_EPM0CAL55_YPT_NUS_MBBLD"
                    , DataPoint Table9 108 "Weekly U.S. Refinery and Blender Net Production of Finished Conventional Motor Gasoline, Greater than Ed 55  (Thousand Barrels per Day)" "W_EPM0CAG55_YPT_NUS_MBBLD"
                    , DataPoint Table9 114 "Weekly U.S. Refinery and Blender Net Production Other Finished Conventional Motor Gasoline  (Thousand Barrels per Day)" "WG6TP_NUS_2"
                    , DataPoint Table9 138 "Weekly U.S. Refinery and Blender Net Production of Distillate Fuel Oil   (Thousand Barrels per Day)" "WDIRPUS2"
                    ]
      table3Data1 = [
                      DataPoint Table3 28 "Weekly U.S. Blender Production of Gasoline Reformulated  (Thousand Barrels per Day)" "W_EPM0R_YPB_NUS_MBBLD"
                    , DataPoint Table3 29 "Weekly U.S. Blender Production of Reformulated Gasoline with Alcohol  (Thousand Barrels per Day)" "W_EPM0RA_YPB_NUS_MBBLD"
                    , DataPoint Table3 30 "Weekly U.S. Blender Net Production of Finished Motor Gasoline Reformulated Other  (Thousand Barrels per Day)" "W_EPM0RO_YPB_NUS_MBBLD"
                    , DataPoint Table3 31 "Weekly U.S. Blender Production of Conventional Gasoline  (Thousand Barrels per Day)" "W_EPM0C_YPB_NUS_MBBLD"
                    , DataPoint Table3 32 "Weekly U.S. Blender Production of Conventional Gasoline with Alcohol  (Thousand Barrels per Day)" "W_EPM0CA_YPB_NUS_MBBLD"
                    , DataPoint Table3 33 "Weekly U.S. Blender Net Production of Motor Gasoline, Finished, Conventional, Ed 55 & <  (Thousand Barrels per Day)" "W_EPM0CAL55_YPB_NUS_MBBLD"
                    , DataPoint Table3 34 "Weekly U.S. Blender Net Production of Motor Gasoline, Finished, Conventional, > Ed 55  (Thousand Barrels per Day)" "W_EPM0CAG55_YPB_NUS_MBBLD"
                    , DataPoint Table3 35 "Weekly U.S. Blender Production of Conventional Gasoline Other  (Thousand Barrels per Day)" "W_EPM0CO_YPB_NUS_MBBLD"
                    ]
      table7Data1 = [
                      DataPoint Table7 21 "Weekly U.S. Total Gasoline Imports  (Thousand Barrels per Day)" "WGTIMUS2"
                    , DataPoint Table7 25 "Weekly U.S. Imports of Fuel Ethanol  (Thousand Barrels per Day)" "W_EPOOXE_IM0_NUS-Z00_MBBLD"
                    ]
      table9Data6 = [
                      DataPoint Table9 274 "Weekly U.S. Ending Stocks of Oxygenates Fuel Ethanol  (Thousand Barrels)" "W_EPOOXE_SAE_NUS_MBBL"
                    , DataPoint Table9 275 "Weekly East Coast (PADD 1) Ending Stocks of Oxygenates Fuel Ethanol  (Thousand Barrels)" "W_EPOOXE_SAE_R10_MBBL"
                    , DataPoint Table9 276 "Weekly Midwest (PADD 2) Ending Stocks of Oxygenates Fuel Ethanol  (Thousand Barrels)" "W_EPOOXE_SAE_R20_MBBL"
                    , DataPoint Table9 277 "Weekly Gulf Coast (PADD 3) Ending Stocks of Oxygenates Fuel Ethanol  (Thousand Barrels)" "W_EPOOXE_SAE_R30_MBBL"
                    , DataPoint Table9 278 "Weekly Rocky Mountain (PADD 4) Ending Stocks of Oxygenates Fuel Ethanol  (Thousand Barrels)" "W_EPOOXE_SAE_R40_MBBL"
                    , DataPoint Table9 279 "Weekly West Coast (PADD 5) Ending Stocks of Oxygenates Fuel Ethanol  (Thousand Barrels)" "W_EPOOXE_SAE_R50_MBBL"
                    , DataPoint Table9 58 "Weekly U.S. Refinery and Blender Net Input of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)" "W_EPOOXE_YIR_NUS_MBBLD"
                    , DataPoint Table9 59 "Weekly East Coast (PADD 1) Refinery and Blender Net Input of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)" "W_EPOOXE_YIR_R10_MBBLD"
                    , DataPoint Table9 60 "Weekly Midwest (PADD 2) Refinery and Blender Net Input of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)" "W_EPOOXE_YIR_R20_MBBLD"
                    , DataPoint Table9 61 "Weekly Gulf Coast (PADD 3) Refinery and Blender Net Input of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)" "W_EPOOXE_YIR_R30_MBBLD"
                    , DataPoint Table9 62 "Weekly Rocky Mountain (PADD 4) Refinery and Blender Net Input of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)" "W_EPOOXE_YIR_R40_MBBLD"
                    , DataPoint Table9 63 "Weekly West Coast (PADD 5) Refinery and Blender Net Input of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)" "W_EPOOXE_YIR_R50_MBBLD"
                    , DataPoint Table9 78 "Weekly U.S. Refinery and Blender Net Production of Finished Reformulated Motor Gasoline with Ethanol   (Thousand Barrels per Day)" "WG1TP_NUS_2"
                    , DataPoint Table9 79 "Weekly East Coast (PADD 1) Refinery and Blender Net Production of Finished Reformulated Motor Gasoline with Ethanol   (Thousand Barrels per Day)" "WG1TP_R10_2"
                    , DataPoint Table9 80 "Weekly Midwest (PADD 2) Refinery and Blender Net Production of Finished Reformulated Motor Gasoline with Ethanol  (Thousand Barrels per Day)" "WG1TP_R20_2"
                    , DataPoint Table9 81 "Weekly Gulf Coast (PADD 3) Refinery and Blender Net Production of Finished Reformulated Motor Gasoline with Ethanol   (Thousand Barrels per Day)" "WG1TP_R30_2"
                    , DataPoint Table9 82 "Weekly Rocky Mountain (PADD 4) Refinery and Blender Net Production of Finished Reformulated Motor Gasoline with Ethanol   (Thousand Barrels per Day)" "WG1TP_R40_2"
                    , DataPoint Table9 83 "Weekly West Coast (PADD 5) Refinery and Blender Net Production of Finished Reformulated Motor Gasoline with Ethanol   (Thousand Barrels per Day)" "WG1TP_R50_2"
                    , DataPoint Table9 96 "Weekly U.S. Refinery and Blender Net Production of Finished Conventional Motor Gasoline with Ethanol  (Thousand Barrels per Day)" "WG5TP_NUS_2"
                    , DataPoint Table9 97 "Weekly East Coast (PADD 1) Refinery and Blender Net Production of Finished Conventional Motor Gasoline with Ethanol  (Thousand Barrels per Day)" "WG5TP_R10_2"
                    , DataPoint Table9 98 "Weekly Midwest (PADD 2) Refinery and Blender Net Production of Finished Conventional Motor Gasoline with Ethanol (Thousand Barrels per Day)" "WG5TP_R20_2"
                    , DataPoint Table9 99 "Weekly Gulf Coast (PADD 3) Refinery and Blender Net Production of Finished Conventional Motor Gasoline with Ethanol  (Thousand Barrels per Day)" "WG5TP_R30_2"
                    , DataPoint Table9 100 "Weekly Rocky Mountain (PADD 4) Refinery and Blender Net Production of Finished Conventional Motor Gasoline with Ethanol  (Thousand Barrels per Day)" "WG5TP_R40_2"
                    , DataPoint Table9 101 "Weekly West Coast (PADD 5) Refinery and Blender Net Production of Finished Conventional Motor Gasoline with Ethanol  (Thousand Barrels per Day)" "WG5TP_R50_2"
                    , DataPoint Table9 102 "Weekly U.S. Refinery and Blender Net Production of Finished Conventional Motor Gasoline, Ed 55 and Lower  (Thousand Barrels per Day)" "W_EPM0CAL55_YPT_NUS_MBBLD"
                    , DataPoint Table9 103 "Weekly East Coast (PADD 1) Refinery and Blender Net Production of Finished Conventional Motor Gasoline, Ed 55 and Lower  (Thousand Barrels per Day)" "W_EPM0CAL55_YPT_R10_MBBLD"
                    , DataPoint Table9 104 "Weekly Midwest (PADD 2) Refinery and Blender Net Production of Finished Conventional Motor Gasoline, Ed 55 and Lower  (Thousand Barrels per Day)" "W_EPM0CAL55_YPT_R20_MBBLD"
                    , DataPoint Table9 105 "Weekly Gulf Coast (PADD 3) Refinery and Blender Net Production of Finished Conventional Motor Gasoline, Ed 55 and Lower  (Thousand Barrels per Day)" "W_EPM0CAL55_YPT_R30_MBBLD"
                    , DataPoint Table9 106 "Weekly Rocky Mountain (PADD 4) Refinery and Blender Net Production of Finished Conventional Motor Gasoline, Ed 55 and Lower  (Thousand Barrels per Day)" "W_EPM0CAL55_YPT_R40_MBBLD"
                    , DataPoint Table9 107 "Weekly West Coast (PADD 5)  Refinery and Blender Net Production of Finished Conventional Motor Gasoline, Ed 55 and Lower  (Thousand Barrels per Day)" "W_EPM0CAL55_YPT_R50_MBBLD"
                    ]
      table2Data11 = [
                       DataPoint Table2 61 "Weekly U.S. Oxy Plant Production of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)" "W_EPOOXE_YOP_NUS_MBBLD"
                     , DataPoint Table2 62 "Weekly East Coast (PADD 1) Oxy Plant Production of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)" "W_EPOOXE_YOP_R10_MBBLD"
                     , DataPoint Table2 63 "Weekly Midwest (PADD 2) Oxy Plant Production of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)" "W_EPOOXE_YOP_R20_MBBLD"
                     , DataPoint Table2 64 "Weekly Gulf Coast (PADD 3) Oxy Plant Production of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)" "W_EPOOXE_YOP_R30_MBBLD"
                     , DataPoint Table2 65 "Weekly Rocky Mountain (PADD 4) Oxy Plant Production of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)" "W_EPOOXE_YOP_R40_MBBLD"
                     , DataPoint Table2 66 "Weekly West Coast (PADD 5) Oxy Plant Production of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)" "W_EPOOXE_YOP_R50_MBBLD"
                     ]
      table9Data2 = [
                      DataPoint Table9 447 "Weekly Fuel Ethanol Imports East Coast (PADD 1)" ""
                    , DataPoint Table9 448 "Weekly Fuel Ethanol Imports Midwest (PADD 2)" ""
                    , DataPoint Table9 449 "Weekly Fuel Ethanol Imports Gulf Coast (PADD 3)" ""
                    , DataPoint Table9 450 "Weekly Fuel Ethanol Imports Rocky Mountain (PADD 4)" ""
                    , DataPoint Table9 451 "Weekly Fuel Ethanol Imports West Coast (PADD 5)" ""
                    ]

