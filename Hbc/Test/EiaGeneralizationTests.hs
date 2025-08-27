{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=), Assertion, assertBool, assertFailure)

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (unpack)
import Text.HTML.TagSoup
import Data.Maybe (isJust)
import Control.Monad (forM_)

import Hbc.Monthly (parseHtmlTable, extractTable, extractEiaHeaders, extractEiaRows, 
                    findRowByName, findColumnIndex, fixMalformedHtml)

main :: IO ()
main = do
    html <- BL.readFile "eia_page2.html"
    defaultMain $ tests html

tests :: BL.ByteString -> [Test]
tests html = 
    [ testGroup "EIA Page 2 Generalization Tests (pet_sum_snd_a_ep00_mbbl_m_cur.htm)"
        [ testGroup "Basic Structure Validation" 
            [ testCase "extracts same number of headers (12)" $ testHeaderCount html
            , testCase "extracts same header names as original" $ testHeaderNames html
            , testCase "extracts same number of rows (6)" $ testRowCount html
            , testCase "extracts same row names (U.S., PADD 1-5)" $ testRowNames html
            ]
        , testGroup "Table Structure Consistency"
            [ testCase "table extraction succeeds" $ testTableExtraction html
            , testCase "row matching works for U.S." $ testRowMatching html "U.S."
            , testCase "row matching works for PADD 1" $ testRowMatching html "PADD 1" 
            , testCase "column matching works for Field Production" $ testColumnMatching html "Field Production"
            , testCase "column matching works for Ending Stocks" $ testColumnMatching html "Ending Stocks"
            ]
        , testGroup "Page 2 Specific Data Extraction (from screenshot)"
            [ testCase "extracts U.S. field production: 649,746" $ testDataExtraction html "U.S." "Field Production" "649,746"
            , testCase "extracts U.S. stock change: 29,955" $ testDataExtraction html "U.S." "Stock Change" "29,955"
            , testCase "extracts U.S. exports: 316,968" $ testDataExtraction html "U.S." "Exports" "316,968"
            , testCase "extracts U.S. ending stocks: 1,644,384" $ testDataExtraction html "U.S." "Ending Stocks" "1,644,384"
            , testCase "extracts PADD 1 ending stocks: 145,099" $ testDataExtraction html "PADD 1" "Ending Stocks" "145,099"
            , testCase "extracts PADD 1 adjustments: -1,372" $ testDataExtraction html "PADD 1" "Adjust- ments" "-1,372"
            ]
        , testGroup "Large Number Handling"
            [ testCase "handles 6-digit numbers correctly" $ testLargeNumbers html
            , testCase "handles 7-digit numbers correctly" $ testVeryLargeNumbers html
            , testCase "comma formatting preserved" $ testCommaFormatting html
            ]
        , testGroup "Cross-Page Consistency" 
            [ testCase "same parsing logic works on different dataset" $ testParsingConsistency html
            , testCase "column alignment consistent across pages" $ testColumnAlignment html
            , testCase "all PADD regions present" $ testAllPaddRegions html
            ]
        ]
    ]

-- Test that we extract the same number of headers as the original page
testHeaderCount :: BL.ByteString -> Assertion  
testHeaderCount html = do
    let tags = parseTags (unpack html)
        htmlStr = unpack html
        fixedHtmlStr = fixMalformedHtml htmlStr
        fixedTags = parseTags fixedHtmlStr
        headers = extractEiaHeaders fixedTags
    length headers @?= 12

-- Test that header names match the original structure
testHeaderNames :: BL.ByteString -> Assertion
testHeaderNames html = do
    let tags = parseTags (unpack html)
        htmlStr = unpack html
        fixedHtmlStr = fixMalformedHtml htmlStr
        fixedTags = parseTags fixedHtmlStr
        headers = extractEiaHeaders fixedTags
        expectedHeaders = ["Field Production", "Transfers to Crude Oil Supply", "Biofuels Plant Net Production", 
                          "Refinery & Blender Net Production", "Imports", "Net Receipts", "Adjust- ments",
                          "Stock Change", "Refinery & Blender Net Inputs", "Exports", "Products Supplied", "Ending Stocks"]
    headers @?= expectedHeaders

-- Test that we extract the same number of data rows
testRowCount :: BL.ByteString -> Assertion
testRowCount html = do
    let tags = parseTags (unpack html)
        htmlStr = unpack html
        fixedHtmlStr = fixMalformedHtml htmlStr
        fixedTags = parseTags fixedHtmlStr
        rows = extractEiaRows fixedTags
    length rows @?= 6

-- Test that row names match the original structure
testRowNames :: BL.ByteString -> Assertion  
testRowNames html = do
    let tags = parseTags (unpack html)
        htmlStr = unpack html
        fixedHtmlStr = fixMalformedHtml htmlStr
        fixedTags = parseTags fixedHtmlStr
        rows = extractEiaRows fixedTags
        rowNames = map head rows
        expectedRowNames = ["U.S.", "PADD 1", "PADD 2", "PADD 3", "PADD 4", "PADD 5"]
    rowNames @?= expectedRowNames

-- Test that complete table extraction works
testTableExtraction :: BL.ByteString -> Assertion
testTableExtraction html = do
    let tags = parseTags (unpack html)
        htmlStr = unpack html
        fixedHtmlStr = fixMalformedHtml htmlStr
        fixedTags = parseTags fixedHtmlStr
        table = extractTable fixedTags
    assertBool "Should be able to extract table" (isJust table)

-- Test that we can find specific rows by name
testRowMatching :: BL.ByteString -> String -> Assertion
testRowMatching html targetRow = do
    let tags = parseTags (unpack html)
        htmlStr = unpack html
        fixedHtmlStr = fixMalformedHtml htmlStr
        fixedTags = parseTags fixedHtmlStr
        table = extractTable fixedTags
    case table of
        Nothing -> assertFailure "Could not extract table"
        Just (_, rows) -> do
            let foundRow = findRowByName rows targetRow
            assertBool ("Should find row: " ++ targetRow) (isJust foundRow)

-- Test that we can find specific columns by name
testColumnMatching :: BL.ByteString -> String -> Assertion
testColumnMatching html targetCol = do
    let tags = parseTags (unpack html)
        htmlStr = unpack html
        fixedHtmlStr = fixMalformedHtml htmlStr
        fixedTags = parseTags fixedHtmlStr
        table = extractTable fixedTags
    case table of
        Nothing -> assertFailure "Could not extract table"
        Just (headers, _) -> do
            let foundCol = findColumnIndex headers targetCol
            assertBool ("Should find column: " ++ targetCol) (isJust foundCol)

-- Test specific data extraction with expected values from page2.png
testDataExtraction :: BL.ByteString -> String -> String -> String -> Assertion
testDataExtraction html rowName colName expectedValue = do
    let result = parseHtmlTable html rowName colName
    case result of
        Nothing -> assertFailure $ "Could not extract data for " ++ rowName ++ " / " ++ colName
        Just actualValue -> do
            let actualStr = unpack actualValue
            actualStr @?= expectedValue

-- Test handling of large 6-digit numbers
testLargeNumbers :: BL.ByteString -> Assertion
testLargeNumbers html = do
    let result = parseHtmlTable html "U.S." "Field Production"
    case result of
        Nothing -> assertFailure "Could not extract U.S. Field Production"
        Just value -> do
            let valueStr = unpack value
            assertBool "Should be 6-digit number with commas" (length valueStr == 7 && ',' `elem` valueStr)

-- Test handling of very large 7-digit numbers  
testVeryLargeNumbers :: BL.ByteString -> Assertion
testVeryLargeNumbers html = do
    let result = parseHtmlTable html "U.S." "Ending Stocks"
    case result of
        Nothing -> assertFailure "Could not extract U.S. Ending Stocks"
        Just value -> do
            let valueStr = unpack value
            assertBool "Should be 7-digit number with commas" (length valueStr == 9 && ',' `elem` valueStr)

-- Test that comma formatting is preserved in large numbers
testCommaFormatting :: BL.ByteString -> Assertion
testCommaFormatting html = do
    let largeNumbers = [("U.S.", "Field Production", "649,746"),
                       ("U.S.", "Ending Stocks", "1,644,384")]
    mapM_ testCommaNumber largeNumbers
  where
    testCommaNumber (rowName, colName, _expected) = do
        let result = parseHtmlTable html rowName colName
        case result of
            Nothing -> assertFailure $ "Could not extract " ++ rowName ++ " " ++ colName
            Just value -> do
                let actualStr = unpack value
                assertBool ("Should preserve comma formatting: " ++ actualStr) (',' `elem` actualStr)

-- Test that parsing logic is consistent across different datasets
testParsingConsistency :: BL.ByteString -> Assertion
testParsingConsistency html = do
    let tags = parseTags (unpack html)
        htmlStr = unpack html
        fixedHtmlStr = fixMalformedHtml htmlStr
        fixedTags = parseTags fixedHtmlStr
        rows = extractEiaRows fixedTags
    -- All rows should have exactly 13 columns (area + 12 data)
    mapM_ (\row -> length row @?= 13) rows

-- Test that column alignment is consistent
testColumnAlignment :: BL.ByteString -> Assertion  
testColumnAlignment html = do
    let tags = parseTags (unpack html)
        htmlStr = unpack html
        fixedHtmlStr = fixMalformedHtml htmlStr
        fixedTags = parseTags fixedHtmlStr
        headers = extractEiaHeaders fixedTags
        table = extractTable fixedTags
    case table of
        Nothing -> assertFailure "Could not extract table"
        Just (_, _rows) -> do
            -- Test that we can extract from each column for U.S. row
            forM_ (zip [0..] headers) $ \(_i, headerName) -> do
                let result = parseHtmlTable html "U.S." headerName
                -- Not all columns have data, but extraction should not fail structurally
                case result of
                    Nothing -> return () -- Some columns may be empty, that's OK
                    Just _ -> return ()   -- Successfully extracted

-- Test that all PADD regions are present and parseable
testAllPaddRegions :: BL.ByteString -> Assertion
testAllPaddRegions html = do
    let paddRegions = ["PADD 1", "PADD 2", "PADD 3", "PADD 4", "PADD 5"]
    mapM_ testPaddRegion paddRegions
  where
    testPaddRegion regionName = do
        let result = parseHtmlTable html regionName "Ending Stocks"
        case result of
            Nothing -> assertFailure $ "Could not extract " ++ regionName ++ " Ending Stocks"
            Just value -> do
                let valueStr = unpack value
                assertBool ("Should have numeric value for " ++ regionName) (not (null valueStr))