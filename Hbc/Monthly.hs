{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Hbc.Monthly where

import Hbc.Scraper

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Vector ((!?))
import Control.Monad.Except

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

getDataPoint :: MonthlySpreadsheets -> DataPoint -> Maybe ByteString
getDataPoint sheets (DataPoint { sheet = s, rowI = r, colI = col }) = do
        csv <- M.lookup s sheets
        row <- csv !? r
        row !? col

getResult :: MonthlySpreadsheets -> DataPoint -> Maybe ResultColumn
getResult sheets d@(DataPoint { description = desc, code = c }) =
        fmap (ResultColumn c desc) (getDataPoint sheets d)

getResults :: MonthlySpreadsheets -> [DataPoint] -> Maybe [ResultColumn]
getResults cols = sequence . fmap (getResult cols)

runBot :: FilePath -> GetData ()
runBot fpath = do
        r <- getSpreadsheets monthlyURLs
        result <- maybe' "Could not find all columns" $ getResults' dataPoints r
        let date = ""
            out = BL.intercalate "\n" $ getOutRows date result
        liftIO $ BL.writeFile fpath out
  where
      maybe' e = maybe (throwError e) return
      getResults' = flip getResults

monthlyURLs :: [(MonthlySpreadsheet, String)]
monthlyURLs = [
                (MonthlyTable1, "http://www.eia.gov/petroleum/supply/monthly/csv/table1.csv")
              , (MonthlyTable5, "http://www.eia.gov/petroleum/supply/monthly/csv/table5.csv")
              , (MonthlyTable9, "http://www.eia.gov/petroleum/supply/monthly/csv/table9.csv")
              , (MonthlyTable13, "http://www.eia.gov/petroleum/supply/monthly/csv/table13.csv")
              , (MonthlyTable17, "http://www.eia.gov/petroleum/supply/monthly/csv/table17.csv")
              , (MonthlyTable21, "http://www.eia.gov/petroleum/supply/monthly/csv/table21.csv")
              ]

dataPoints :: [DataPoint]
dataPoints = concat [table1, table5, table9, table13, table17, table21,
                     additions_9_14]
  where
      table1 = [
                 DataPoint MonthlyTable1 14 2 "U.S. Renewable Fuels Plant and Oxygenate Plant Net Production of Fuel Ethanol (Thousand Barrels)" "M_EPOOXE_YNP_NUS_MBBL"
               , DataPoint MonthlyTable1 14 4 "U.S. Imports of Fuel Ethanol (Thousand Barrels)" "MFEIMUS1"
               , DataPoint MonthlyTable1 14 5 "U.S. Supply Adjustment of Fuel Ethanol (Thousand Barrels)" "M_EPOOXE_VUA_NUS_MBBL"
               , DataPoint MonthlyTable1 14 6 "U.S. Fuel Ethanol Stock Change (Thousand Barrels)" "M_EPOOXE_SCG_NUS_MBBL"
               , DataPoint MonthlyTable1 14 7 "U.S. Refinery and Blender Net Input of Fuel Ethanol (Thousand Barrels)" "MFERIUS1"
               , DataPoint MonthlyTable1 14 9 "U.S. Product Supplied of Fuel Ethanol (Thousand Barrels)" "M_EPOOXE_VPP_NUS_MBBL"
               , DataPoint MonthlyTable1 14 10 "U.S. Ending Stocks of Fuel Ethanol (Thousand Barrels)" "MFESTUS1"
               , DataPoint MonthlyTable1 23 9 "U.S. Product Supplied of Finished Motor Gasoline (Thousand Barrels)" "MGFUPUS1"
               ]
      table5 = [
                 DataPoint MonthlyTable5 14 2 "East Coast (PADD 1) Renewable Fuels Plant and Oxygenate Plant Net Production of Fuel Ethanol (Thousand Barrels)" "M_EPOOXE_YNP_R10_MBBL"
               , DataPoint MonthlyTable5 14 4 "East Coast (PADD 1) Imports of Fuel Ethanol (Thousand Barrels)" "MFEIM_R10-Z00_1"
               , DataPoint MonthlyTable5 14 5 "East Coast (PADD 1) Net Receipts by Pipeline, Tanker, and Barge from Other PADDs of Fuel Ethanol (Thousand Barrels)" "M_EPOOXE_VNR_R10-Z0P_MBBL"
               , DataPoint MonthlyTable5 14 6 "East Coast (PADD 1) Supply Adjustment of Fuel Ethanol (Thousand Barrels)" "M_EPOOXE_VUA_R10_MBBL"
               , DataPoint MonthlyTable5 14 7 "East Coast (PADD 1) Fuel Ethanol Stock Change (Thousand Barrels)" "M_EPOOXE_SCG_R10_MBBL"
               , DataPoint MonthlyTable5 14 8 "East Coast (PADD 1) Refinery and Blender Net Input of Fuel Ethanol (Thousand Barrels)" "MFERIP11"
               , DataPoint MonthlyTable5 14 10 "East Coast (PADD 1) Product Supplied of Fuel Ethanol (Thousand Barrels)" "M_EPOOXE_VPP_R10_MBBL"
               , DataPoint MonthlyTable5 14 11 "East Coast (PADD 1) Ending Stocks of Fuel Ethanol (Thousand Barrels)" "MFESTP11"
               , DataPoint MonthlyTable5 23 10 "East Coast (PADD 1) Product Supplied of Finished Motor Gasoline (Thousand Barrels)" "MGFUPP11"
               ]
      table9 = [
                 DataPoint MonthlyTable9 14 2 "Midwest (PADD 2) Renewable Fuels Plant and Oxygenate Plant Net Production of Fuel Ethanol (Thousand Barrels)" "M_EPOOXE_YNP_R20_MBBL"
               , DataPoint MonthlyTable9 14 4 "Midwest (PADD 2) Imports of Fuel Ethanol (Thousand Barrels)" "MFEIMP21"
               , DataPoint MonthlyTable9 14 5 "Midwest (PADD 2) Net Receipts by Pipeline, Tanker, and Barge from Other PADDs of Fuel Ethanol (Thousand Barrels)" "M_EPOOXE_VNR_R20-Z0P_MBBL"
               , DataPoint MonthlyTable9 14 6 "Midwest (PADD 2) Supply Adjustment of Fuel Ethanol (Thousand Barrels)" "M_EPOOXE_VUA_R20_MBBL"
               , DataPoint MonthlyTable9 14 7 "Midwest (PADD 2) Fuel Ethanol Stock Change (Thousand Barrels)" "M_EPOOXE_SCG_R20_MBBL"
               , DataPoint MonthlyTable9 14 8 "Midwest (PADD 2) Refinery and Blender Net Input of Fuel Ethanol (Thousand Barrels)" "MFERIP21"
               , DataPoint MonthlyTable9 14 10 "Midwest (PADD 2) Product Supplied of Fuel Ethanol (Thousand Barrels)" "M_EPOOXE_VPP_R20_MBBL"
               , DataPoint MonthlyTable9 14 11 "Midwest (PADD 2) Ending Stocks of Fuel Ethanol (Thousand Barrels)" "MFESTP21"
               , DataPoint MonthlyTable9 23 10 "Midwest (PADD 2) Product Supplied of Finished Motor Gasoline (Thousand Barrels)" "MGFUPP21"
               ]
      table13 = [
                  DataPoint MonthlyTable13 14 2 "Gulf Coast (PADD 3) Renewable Fuels Plant and Oxygenate Plant Net Production of Fuel Ethanol (Thousand Barrels)" "M_EPOOXE_YNP_R30_MBBL"
                , DataPoint MonthlyTable13 14 4 "Gulf Coast (PADD 3) Imports of Fuel Ethanol (Thousand Barrels)" "MFEIMP31"
                , DataPoint MonthlyTable13 14 5 "Gulf Coast (PADD 3) Net Receipts by Pipeline, Tanker, and Barge from Other PADDs of Fuel Ethanol (Thousand Barrels)" "M_EPOOXE_VNR_R30-Z0P_MBBL"
                , DataPoint MonthlyTable13 14 6 "Gulf Coast (PADD 3) Supply Adjustment of Fuel Ethanol (Thousand Barrels)" "M_EPOOXE_VUA_R30_MBBL"
                , DataPoint MonthlyTable13 14 7 "Gulf Coast (PADD 3) Fuel Ethanol Stock Change (Thousand Barrels)" "M_EPOOXE_SCG_R30_MBBL"
                , DataPoint MonthlyTable13 14 8 "Gulf Coast (PADD 3) Refinery and Blender Net Input of Fuel Ethanol (Thousand Barrels)" "MFERIP31"
                , DataPoint MonthlyTable13 14 10 "Gulf Coast (PADD 3) Product Supplied of Fuel Ethanol (Thousand Barrels)" "M_EPOOXE_VPP_R30_MBBL"
                , DataPoint MonthlyTable13 14 11 "Gulf Coast (PADD 3) Ending Stocks of Fuel Ethanol (Thousand Barrels)" "MFESTP31"
                , DataPoint MonthlyTable13 23 10 "Gulf Coast (PADD 3) Product Supplied of Finished Motor Gasoline (Thousand Barrels)" "MGFUPP31"
                ]
      table17 = [
                  DataPoint MonthlyTable17 14 2 "Rocky Mountain (PADD 4) Renewable Fuels Plant and Oxygenate Plant Net Production of Fuel Ethanol (Thousand Barrels)" "M_EPOOXE_YNP_R40_MBBL"
                , DataPoint MonthlyTable17 14 4 "Rocky Mountain (PADD 4) Imports of Fuel Ethanol (Thousand Barrels)" "MFEIM_R40-Z00_1"
                , DataPoint MonthlyTable17 14 5 "Rocky Mountain (PADD 4) Net Receipts by Pipeline, Tanker, and Barge from Other PADDs of Fuel Ethanol (Thousand Barrels)" "M_EPOOXE_VNR_R40-Z0P_MBBL"
                , DataPoint MonthlyTable17 14 6 "Rocky Mountain (PADD 4) Supply Adjustment of Fuel Ethanol (Thousand Barrels)" "M_EPOOXE_VUA_R40_MBBL"
                , DataPoint MonthlyTable17 14 7 "Rocky Mountain (PADD 4) Fuel Ethanol Stock Change (Thousand Barrels)" "M_EPOOXE_SCG_R40_MBBL"
                , DataPoint MonthlyTable17 14 8 "Rocky Mountain (PADD 4) Refinery and Blender Net Input of Fuel Ethanol (Thousand Barrels)" "MFERIP41"
                , DataPoint MonthlyTable17 14 10 "Rocky Mountain (PADD 4) Product Supplied of Fuel Ethanol (Thousand Barrels)" "M_EPOOXE_VPP_R40_MBBL"
                , DataPoint MonthlyTable17 14 11 "Rocky Mountain (PADD 4) Ending Stocks of Fuel Ethanol (Thousand Barrels)" "MFESTP41"
                , DataPoint MonthlyTable17 23 10 "Rocky Mountain (PADD 4) Product Supplied of Finished Motor Gasoline (Thousand Barrels)" "MGFUPP41"
                ]
      table21 = [
                  DataPoint MonthlyTable21 14 2 "West Coast (PADD 5) Renewable Fuels Plant and Oxygenate Plant Net Production of Fuel Ethanol (Thousand Barrels)" "M_EPOOXE_YNP_R50_MBBL"
                , DataPoint MonthlyTable21 14 4 "West Coast (PADD 5) Imports of Fuel Ethanol (Thousand Barrels)" "MFEIMP51"
                , DataPoint MonthlyTable21 14 5 "West Coast (PADD 5) Net Receipts by Pipeline, Tanker, and Barge from Other PADDs of Fuel Ethanol (Thousand Barrels)" "M_EPOOXE_VNR_R50-Z0P_MBBL"
                , DataPoint MonthlyTable21 14 6 "West Coast (PADD 5) Supply Adjustment of Fuel Ethanol (Thousand Barrels)" "M_EPOOXE_VUA_R50_MBBL"
                , DataPoint MonthlyTable21 14 7 "West Coast (PADD 5) Fuel Ethanol Stock Change (Thousand Barrels)" "M_EPOOXE_SCG_R50_MBBL"
                , DataPoint MonthlyTable21 14 8 "West Coast (PADD 5) Refinery and Blender Net Input of Fuel Ethanol (Thousand Barrels)" "MFERIP51"
                , DataPoint MonthlyTable21 14 10 "West Coast (PADD 5) Product Supplied of Fuel Ethanol (Thousand Barrels)" "M_EPOOXE_VPP_R50_MBBL"
                , DataPoint MonthlyTable21 14 11 "West Coast (PADD 5) Ending Stocks of Fuel Ethanol (Thousand Barrels)" "MFESTP51"
                , DataPoint MonthlyTable21 23 10 "West Coast (PADD 5) Product Supplied of Finished Motor Gasoline (Thousand Barrels)" "MGFUPP51"
                ]
      additions_9_14 = [
                        DataPoint MonthlyTable1 17 8 "U.S. Exports of Unfinished Oils (Thousand Barrels)" "MUOEXUS1"
                      , DataPoint MonthlyTable1 18 8 "U.S. Exports of Gasoline Blending Components (Thousand Barrels)" "MBCEXUS1"
                      , DataPoint MonthlyTable1 19 8 "U.S. Exports of Reformulated Gasoline Blending Components (Thousand Barrels)" "MO1EX_NUS-Z00_1"
                      , DataPoint MonthlyTable1 20 8 "U.S. Exports of Conventional Gasoline Blending Components (Thousand Barrels)" "MO5EX_NUS-Z00_1"
                      , DataPoint MonthlyTable1 23 8 "U.S. Exports of Finished Motor Gasoline (Thousand Barrels)" "MGFEXUS1"
                      , DataPoint MonthlyTable1 24 8 "U.S. Exports of Reformulated Motor Gasoline (Thousand Barrels)" "MGREXUS1"
                      , DataPoint MonthlyTable1 25 8 "U.S. Exports of Conventional Motor Gasoline (Thousand Barrels)" "MG4EX_NUS-Z00_1"
                      ]

