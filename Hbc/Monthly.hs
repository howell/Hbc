module Hbc.Monthly where

import Hbc.Scraper

data MonthlySpreadsheet = MonthlyTable1
                        | MonthlyTable5
                        | MonthlyTable9
                        | MonthlyTable13
                        | MonthlyTable17
                        | MonthlyTable21 deriving (Eq, Ord, Show, Read)

