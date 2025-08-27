{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=), Assertion, assertBool, assertFailure)

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (unpack)
import Text.HTML.TagSoup
import Data.Maybe (isJust, isNothing)
import Data.List (find, isInfixOf)
import Control.Monad (forM_)

import Hbc.Monthly (parseHtmlTable, extractTable, extractEiaHeaders, extractEiaRows, 
                    findRowByName, findColumnIndex, extractEiaDataCells,
                    parseHeaderRow, buildHeaderMatrix, resolveHeaderList, HeaderCell(..),
                    fixMalformedHtml)

main :: IO ()
main = do
    html <- BL.readFile "eia_sample_page.html"
    defaultMain (tests html)

tests :: BL.ByteString -> [Test]
tests html = 
    [ testGroup "EIA HTML Table Parsing Tests"
        [ testGroup "Basic Structure Extraction" 
            [ testCase "extracts correct number of headers" $ testHeaderCount html
            , testCase "extracts expected header names" $ testHeaderNames html
            , testCase "extracts correct number of rows" $ testRowCount html
            , testCase "extracts expected row names" $ testRowNames html
            ]
        , testGroup "Table Structure Validation"
            [ testCase "table extraction succeeds" $ testTableExtraction html
            , testCase "row matching works for U.S." $ testRowMatching html "U.S."
            , testCase "row matching works for PADD 1" $ testRowMatching html "PADD 1" 
            , testCase "row matching works for PADD 2" $ testRowMatching html "PADD 2"
            , testCase "column matching works for Field Production" $ testColumnMatching html "Field Production"
            , testCase "column matching works for Imports" $ testColumnMatching html "Imports"
            , testCase "column matching works for Exports" $ testColumnMatching html "Exports"
            , testCase "column matching works for Ending Stocks" $ testColumnMatching html "Ending Stocks"
            ]
        , testGroup "Data Point Extraction"
            [ testCase "extracts U.S. stock change value" $ testDataExtraction html "U.S." "Stock Change" "-668"
            , testCase "extracts U.S. exports value" $ testDataExtraction html "U.S." "Exports" "4,410"
            , testCase "handles U.S. imports (empty/dash)" $ testEmptyDataExtraction html "U.S." "Imports"
            , testCase "handles PADD 1 imports (empty/dash)" $ testEmptyDataExtraction html "PADD 1" "Imports"
            , testCase "extracts PADD 1 ending stocks value" $ testDataExtraction html "PADD 1" "Ending Stocks" "7,885"
            , testCase "handles non-existent data points gracefully" $ testMissingData html "NonExistent" "FakeColumn"
            ]
        , testGroup "Real-world Data Validation" 
            [ testCase "all expected U.S. data points exist" $ testUsDataPoints html
            , testCase "all PADD adjustments values are correct" $ testAllPaddRegions html
            , testCase "numeric values are reasonable" $ testDataSanity html
            ]
        , testGroup "Individual Row Extraction Tests"
            [ testCase "extracts PADD 1 row with correct values" $ testIndividualRowExtraction
            , testCase "extracts correct number of TDs from individual row" $ testIndividualRowTDCount
            , testCase "identifies area name correctly from individual row" $ testIndividualRowAreaName
            ]
        , testGroup "Direct Header Parsing Tests"
            [ testCase "parses first header row with colspan/rowspan" $ testParseFirstHeaderRow
            , testCase "parses second header row with individual headers" $ testParseSecondHeaderRow
            , testCase "resolves two-row header matrix to 12 final headers" $ testResolveHeaderMatrix
            ]
        , testGroup "Full-Page Integration Debugging"
            [ testCase "debug full-page header extraction" $ testFullPageHeaders html
            , testCase "debug full-page row extraction" $ testFullPageRows html
            , testCase "debug PADD 1 row structure" $ testPadd1RowStructure html
            , testCase "debug U.S. row structure" $ testUsRowStructure html
            , testCase "debug column index matching" $ testColumnIndexMatching html
            ]
        ]
    ]

-- Test that we extract the expected number of headers
testHeaderCount :: BL.ByteString -> Assertion  
testHeaderCount html = do
    let tags = parseTags (unpack html)
        headers = extractEiaHeaders tags
    length headers @?= 12

-- Test that we get the expected header names
testHeaderNames :: BL.ByteString -> Assertion
testHeaderNames html = do
    let tags = parseTags (unpack html)  
        headers = extractEiaHeaders tags
        expectedHeaders = ["Field Production", "Transfers to Crude Oil Supply", "Biofuels Plant Net Production", 
                          "Refinery & Blender Net Production", "Imports", "Net Receipts", "Adjust- ments",
                          "Stock Change", "Refinery & Blender Net Inputs", "Exports", "Products Supplied", "Ending Stocks"]
    headers @?= expectedHeaders

-- Test that we extract the expected number of data rows
testRowCount :: BL.ByteString -> Assertion
testRowCount html = do
    let tags = parseTags (unpack html)
        rows = extractEiaRows tags
    length rows @?= 6

-- Test that we get the expected row names
testRowNames :: BL.ByteString -> Assertion  
testRowNames html = do
    let tags = parseTags (unpack html)
        -- Apply the same HTML fix that extractTable uses
        htmlStr = unpack html
        fixedHtmlStr = fixMalformedHtml htmlStr
        fixedTags = parseTags fixedHtmlStr
        rows = extractEiaRows fixedTags
        rowNames = map head rows  -- First element should be row name
        expectedRowNames = ["U.S.", "PADD 1", "PADD 2", "PADD 3", "PADD 4", "PADD 5"]
    rowNames @?= expectedRowNames

-- Test that complete table extraction works
testTableExtraction :: BL.ByteString -> Assertion
testTableExtraction html = do
    let tags = parseTags (unpack html)
        result = extractTable tags
    case result of
        Nothing -> assertFailure "Table extraction failed"
        Just (headers, rows) -> do
            length headers @?= 12
            length rows @?= 6

-- Test that row matching works for a specific row
testRowMatching :: BL.ByteString -> String -> Assertion
testRowMatching html targetRow = do
    let tags = parseTags (unpack html)
        result = extractTable tags
    case result of
        Nothing -> assertFailure "Table extraction failed"
        Just (_, rows) -> do
            let match = findRowByName rows targetRow
            assertBool ("Could not find row: " ++ targetRow) (isJust match)

-- Test that column matching works for a specific column
testColumnMatching :: BL.ByteString -> String -> Assertion  
testColumnMatching html targetCol = do
    let tags = parseTags (unpack html)
        headers = extractEiaHeaders tags
        match = findColumnIndex headers targetCol
    assertBool ("Could not find column: " ++ targetCol) (isJust match)

-- Test that we can extract specific data points with expected values
testDataExtraction :: BL.ByteString -> String -> String -> String -> Assertion
testDataExtraction html rowName colName expectedValue = do
    let result = parseHtmlTable html rowName colName
        -- Debug: check what the extraction actually found
        tags = parseTags (unpack html)
        maybeTable = extractTable tags
    case maybeTable of
        Nothing -> assertFailure "Could not extract table"
        Just (headers, rows) -> do
            let maybeRow = findRowByName rows rowName
                maybeColIndex = findColumnIndex headers colName
            putStrLn $ "=== EXTRACTION DEBUG for " ++ rowName ++ " / " ++ colName ++ " ==="
            putStrLn $ "Headers: " ++ show headers
            putStrLn $ "Column index for '" ++ colName ++ "': " ++ show maybeColIndex
            case maybeRow of
                Nothing -> putStrLn $ "Row '" ++ rowName ++ "' not found!"
                Just row -> do
                    putStrLn $ "Row '" ++ rowName ++ "' length: " ++ show (length row)
                    putStrLn $ "Row '" ++ rowName ++ "' content: " ++ show row
                    case maybeColIndex of
                        Nothing -> putStrLn $ "Column '" ++ colName ++ "' not found!"
                        Just colIndex -> do
                            let cellValue = if colIndex < length row then row !! colIndex else "OUT_OF_BOUNDS"
                            putStrLn $ "Cell at index " ++ show colIndex ++ ": '" ++ cellValue ++ "'"
    case result of
        Nothing -> assertFailure $ "Could not extract data for " ++ rowName ++ " / " ++ colName
        Just actualValue -> do
            let actualStr = unpack actualValue
            actualStr @?= expectedValue

-- Test that missing data points return Nothing gracefully
testMissingData :: BL.ByteString -> String -> String -> Assertion
testMissingData html rowName colName = do
    let result = parseHtmlTable html rowName colName
    assertBool "Expected Nothing for non-existent data point" (isNothing result)

-- Test that empty data points are handled correctly (returns Just "" for empty but valid cells)
testEmptyDataExtraction :: BL.ByteString -> String -> String -> Assertion  
testEmptyDataExtraction html rowName colName = do
    let result = parseHtmlTable html rowName colName
    -- For empty cells that exist, parseHtmlTable should return Just "" 
    case result of
        Just value -> unpack value @?= ""  -- Should be empty string
        Nothing -> assertFailure $ "Expected Just \"\" for empty but valid cell " ++ rowName ++ " / " ++ colName

-- Test that all expected U.S. data points can be found
testUsDataPoints :: BL.ByteString -> Assertion
testUsDataPoints html = do
    let usDataPoints = [ ("Stock Change", "-668")
                       , ("Exports", "4,410")
                       ]
    mapM_ testUsDataPoint usDataPoints
  where
    testUsDataPoint (colName, expectedValue) = do
        let result = parseHtmlTable html "U.S." colName
        case result of
            Nothing -> assertFailure $ "Could not find U.S. " ++ colName
            Just actualValue -> do
                let actualStr = unpack actualValue
                actualStr @?= expectedValue

-- Test that all PADD regions have data (using Adjust- ments which has values for all)
testAllPaddRegions :: BL.ByteString -> Assertion  
testAllPaddRegions html = do
    let paddDataPoints = [ ("PADD 1", "352")
                         , ("PADD 2", "-1,521") 
                         , ("PADD 3", "1,332")
                         , ("PADD 4", "478")
                         , ("PADD 5", "-364")
                         ]
        testColumn = "Adjust- ments"
    mapM_ (testPaddAdjustments testColumn) paddDataPoints
  where
    testPaddAdjustments colName (rowName, expectedValue) = do
        let result = parseHtmlTable html rowName colName
        case result of
            Nothing -> assertFailure $ "Could not find " ++ rowName ++ " " ++ colName
            Just actualValue -> do
                let actualStr = unpack actualValue
                actualStr @?= expectedValue

-- Test that numeric values look reasonable (not empty, not obviously wrong)
testDataSanity :: BL.ByteString -> Assertion
testDataSanity html = do
    let result = parseHtmlTable html "U.S." "Stock Change" 
    case result of
        Nothing -> assertFailure "Could not extract U.S. Stock Change for sanity check"
        Just value -> do
            let valueStr = unpack value
            -- Should be a number (possibly negative, with commas)
            assertBool ("Value doesn't look like a number: " ++ valueStr) 
                       (isReasonableNumber valueStr)
  where
    isReasonableNumber :: String -> Bool
    isReasonableNumber str = 
        not (null str) && 
        all (\c -> c `elem` ("0123456789,-" :: String)) str &&
        length str > 0 && length str < 20

-- First header row: Real complex HTML from EIA page
firstHeaderRowHTML :: String
firstHeaderRowHTML = unlines [
    "<tr>",
    "  <th rowspan=\"2\" valign=\"bottom\">",
    "    <table border=\"0\" cellspacing=\"0\" cellpadding=\"0\" align=\"left\" class=\"data2\">",
    "      <tr>",
    "        <td colspan=\"9\" class=\"ViewLabel\">&nbsp;Show Data By:</td>",
    "      </tr>",
    "      <tr>",
    "        <td height=\"30\" colspan=\"9\"></td>",
    "      </tr>",
    "      <tr align=\"left\">",
    "        <td width=\"15\"><img src=\"img/spacer_transp.gif\" alt=\"\" width=\"1\" height=\"1\" border=\"0\"></td>",
    "        <td align=\"left\"><div id=\"view\" name=\"view\">",
    "          <a href=\"pet_sum_snd_d_nus_mbbl_m_cur.htm\"><img src=\"img/Radio_I9.jpg\" alt=\"\" width=\"13\" height=\"13\" border=\"0\"></a></div></td>",
    "        <td width=\"5\"><img src=\"img/spacer_transp.gif\" alt=\"\" width=\"1\" height=\"1\" border=\"0\"></td>",
    "        <td class=\"LabelB\"><div id=\"view\" name=\"view\"><a href=\"pet_sum_snd_d_nus_mbbl_m_cur.htm\" class=\"NavChunkA\">Product</a></div></td>",
    "        <td width=\"15\"><img src=\"img/spacer_transp.gif\" alt=\"\" width=\"1\" height=\"1\" border=\"0\"></td>",
    "        <td align=\"left\"><div id=\"view\" name=\"view\">",
    "          <a href=\"pet_sum_snd_a_EPOOXE_mbbl_m_cur.htm\"><img src=\"img/Radio_A9.jpg\" alt=\"\" width=\"13\" height=\"13\" border=\"0\"></a></div></td>",
    "        <td width=\"5\"><img src=\"img/spacer_transp.gif\" alt=\"\" width=\"1\" height=\"1\" border=\"0\"></td>",
    "        <td class=\"LabelB\">",
    "          <a href=\"pet_sum_snd_a_EPOOXE_mbbl_m_cur.htm\" class=\"NavChunkA\">Area</a></td>",
    "        <td width=\"10\"><img src=\"img/spacer_transp.gif\" alt=\"\" width=\"1\" height=\"1\" border=\"0\"></td>",
    "      </tr>",
    "      <tr>",
    "        <td height=\"5\" colspan=\"9\"></td>",
    "      </tr>",
    "    </table>",
    "    <img src=\"img/spacer_transp.gif\" alt=\"\" width=\"1\" height=\"1\" border=\"0\">",
    "  </th>",
    "  <th colspan=\"7\" class=\"Series3\">Supply</th>",
    "  <th colspan=\"4\" class=\"Series3B\">Disposition</th>",
    "  <th class=\"Cross3\" rowspan=\"2\">Ending Stocks</th>",
    "</tr>"
  ]

-- Second header row: 11 individual column headers
secondHeaderRowHTML :: String  
secondHeaderRowHTML = unlines [
    "<tr>",
    "  <th class=\"Cross\">Field Production</th>",
    "  <th class=\"Cross\">Transfers to Crude Oil Supply</th>",
    "  <th class=\"Cross\">Biofuels Plant Net Production</th>",
    "  <th class=\"Cross\">Refinery & Blender Net Production</th>",
    "  <th class=\"Cross\">Imports</th>",
    "  <th class=\"Cross\">Net Receipts</th>",
    "  <th class=\"Cross\">Adjust- ments</th>",
    "  <th class=\"Cross2\">Stock Change</th>",
    "  <th class=\"Cross2\">Refinery & Blender Net Inputs</th>",
    "  <th class=\"Cross2\">Exports</th>",
    "  <th class=\"Cross2\">Products Supplied</th>",
    "</tr>"
  ]

-- Simplified wrapper for legacy compatibility  
twoRowHeaderHTML :: String
twoRowHeaderHTML = unlines [
    "<table>",
    "<tr>",
    "  <th rowspan=\"2\" valign=\"bottom\">",
    "    <table border=\"0\" cellspacing=\"0\" cellpadding=\"0\" align=\"left\" class=\"data2\">",
    "      <tr>",
    "        <td colspan=\"9\" class=\"ViewLabel\">&nbsp;Show Data By:</td>",
    "      </tr>",
    "      <tr>",
    "        <td height=\"30\" colspan=\"9\"></td>",
    "      </tr>",
    "      <tr align=\"left\">",
    "        <td width=\"15\"><img src=\"img/spacer_transp.gif\" alt=\"\" width=\"1\" height=\"1\" border=\"0\"></td>",
    "        <td align=\"left\"><div id=\"view\" name=\"view\">",
    "          <a href=\"pet_sum_snd_d_nus_mbbl_m_cur.htm\"><img src=\"img/Radio_I9.jpg\" alt=\"\" width=\"13\" height=\"13\" border=\"0\"></a></div></td>",
    "        <td width=\"5\"><img src=\"img/spacer_transp.gif\" alt=\"\" width=\"1\" height=\"1\" border=\"0\"></td>",
    "        <td class=\"LabelB\"><div id=\"view\" name=\"view\"><a href=\"pet_sum_snd_d_nus_mbbl_m_cur.htm\" class=\"NavChunkA\">Product</a></div></td>",
    "        <td width=\"15\"><img src=\"img/spacer_transp.gif\" alt=\"\" width=\"1\" height=\"1\" border=\"0\"></td>",
    "        <td align=\"left\"><div id=\"view\" name=\"view\">",
    "          <a href=\"pet_sum_snd_a_EPOOXE_mbbl_m_cur.htm\"><img src=\"img/Radio_A9.jpg\" alt=\"\" width=\"13\" height=\"13\" border=\"0\"></a></div></td>",
    "        <td width=\"5\"><img src=\"img/spacer_transp.gif\" alt=\"\" width=\"1\" height=\"1\" border=\"0\"></td>",
    "        <td class=\"LabelB\">",
    "          <a href=\"pet_sum_snd_a_EPOOXE_mbbl_m_cur.htm\" class=\"NavChunkA\">Area</a></td>",
    "        <td width=\"10\"><img src=\"img/spacer_transp.gif\" alt=\"\" width=\"1\" height=\"1\" border=\"0\"></td>",
    "      </tr>",
    "      <tr>",
    "        <td height=\"5\" colspan=\"9\"></td>",
    "      </tr>",
    "    </table>",
    "    <img src=\"img/spacer_transp.gif\" alt=\"\" width=\"1\" height=\"1\" border=\"0\">",
    "  </th>",
    "  <th colspan=\"7\" class=\"Series3\">Supply</th>",
    "  <th colspan=\"4\" class=\"Series3B\">Disposition</th>",
    "  <th class=\"Cross3\" rowspan=\"2\">Ending Stocks</th>",
    "</tr>",
    "<tr>",
    "  <th class=\"Cross\">Field Production</th>",
    "  <th class=\"Cross\">Transfers to Crude Oil Supply</th>",
    "  <th class=\"Cross\">Biofuels Plant Net Production</th>",
    "  <th class=\"Cross\">Refinery & Blender Net Production</th>",
    "  <th class=\"Cross\">Imports</th>",
    "  <th class=\"Cross\">Net Receipts</th>",
    "  <th class=\"Cross\">Adjust- ments</th>",
    "  <th class=\"Cross2\">Stock Change</th>",
    "  <th class=\"Cross2\">Refinery & Blender Net Inputs</th>",
    "  <th class=\"Cross2\">Exports</th>",
    "  <th class=\"Cross2\">Products Supplied</th>",
    "</tr>",
    "<tr class=\"DataRow\">",
    "  <td>Test Row</td>",
    "  <td>1</td><td>2</td><td>3</td><td>4</td><td>5</td><td>6</td><td>7</td><td>8</td><td>9</td><td>10</td><td>11</td><td>12</td>",
    "</tr>",
    "</table>"
  ]

-- Individual row HTML excerpt from actual EIA page
individualRowHTML :: String
individualRowHTML = unlines [
    "<tr class=\"DataRow\">",
    "    <td width=\"240\" class=\"DataStub\">",
    "        <table border=\"0\" cellspacing=\"0\" cellpadding=\"0\" class=\"data2\">",
    "            <tr>",
    "                <td width=\"3\"></td>",
    "                <td width=\"237\" class=\"DataStub\">PADD 1</td>",
    "            </tr>",
    "        </table>",
    "    </td>",
    "    <TD class=DataA> </TD>",
    "    <TD class=DataA> </TD>",
    "    <TD class=DataA><a href=./hist/LeafHandler.ashx?n=PET&s=M_EPOOXE_YNP_R10_MBBL&f=M class=data1>374</a></TD>",
    "    <TD class=DataA> </TD>",
    "    <TD class=DataA><a href=./hist/LeafHandler.ashx?n=PET&s=MFEIM_R10-Z00_1&f=M class=data1>-</a></TD>",
    "    <TD class=DataA><a href=./hist/LeafHandler.ashx?n=PET&s=M_EPOOXE_VNR_R10-Z0P_MBBL&f=M class=data1>10,101</a></TD>",
    "    <TD class=DataA><a href=./hist/LeafHandler.ashx?n=PET&s=M_EPOOXE_VUA_R10_MBBL&f=M class=data1>352</a></TD>",
    "    <TD class=DataS><a href=./hist/LeafHandler.ashx?n=PET&s=M_EPOOXE_SCG_R10_MBBL&f=M class=data1>266</a></TD>",
    "    <TD class=DataS><a href=./hist/LeafHandler.ashx?n=PET&s=MFERIP11&f=M class=data1>10,371</a></TD>",
    "    <TD class=DataS><a href=./hist/LeafHandler.ashx?n=PET&s=M_EPOOXE_EEX_R10-Z00_MBBL&f=M class=data1>190</a></TD>",
    "    <TD class=DataS><a href=./hist/LeafHandler.ashx?n=PET&s=M_EPOOXE_VPP_R10_MBBL&f=M class=data1>0</a></TD>",
    "    <TD class=DataA><a href=./hist/LeafHandler.ashx?n=PET&s=MFESTP11&f=M class=data1>7,885</a></TD>",
    "</tr>"
  ]

-- Test extraction from individual row HTML excerpt
testIndividualRowExtraction :: Assertion
testIndividualRowExtraction = do
    let tags = parseTags individualRowHTML
        result = extractEiaDataCells tags
        expected = ["PADD 1","","","374","","","10,101","352","266","10,371","190","0","7,885"]
    result @?= expected

-- Test that we extract correct number of TDs from individual row
testIndividualRowTDCount :: Assertion  
testIndividualRowTDCount = do
    let tags = parseTags individualRowHTML
        result = extractEiaDataCells tags
        expectedCount = 13  -- 1 area + 12 data columns
    length result @?= expectedCount

-- Test that we correctly identify area name from individual row
testIndividualRowAreaName :: Assertion
testIndividualRowAreaName = do  
    let tags = parseTags individualRowHTML
        result = extractEiaDataCells tags
        areaName = case result of
            [] -> ""
            (first:_) -> first
        expected = "PADD 1"
    areaName @?= expected

-- Test parsing first header row directly (focusing on colspan/rowspan handling)
testParseFirstHeaderRow :: Assertion
testParseFirstHeaderRow = do
    let tags = parseTags firstHeaderRowHTML
        headerCells = parseHeaderRow tags
        expectedCount = 4  -- Complex nav area, Supply, Disposition, Ending Stocks
        headerTexts = map headerText headerCells
    -- Debug: show what headers we actually found
    length headerCells @?= expectedCount
    -- Verify Supply has colspan=7
    let supplyHeader = find (\cell -> "Supply" `elem` words (headerText cell)) headerCells
        dispositionHeader = find (\cell -> "Disposition" `elem` words (headerText cell)) headerCells
        endingStocksHeader = find (\cell -> 
            let text = headerText cell
            in "Ending" `elem` words text && "Stocks" `elem` words text) headerCells
    case supplyHeader of
        Nothing -> assertFailure $ "Could not find Supply header. Found: " ++ show headerTexts
        Just cell -> colSpan cell @?= 7
    case dispositionHeader of
        Nothing -> assertFailure $ "Could not find Disposition header. Found: " ++ show headerTexts
        Just cell -> colSpan cell @?= 4
    case endingStocksHeader of
        Nothing -> assertFailure $ "Could not find Ending Stocks header. Found: " ++ show headerTexts
        Just cell -> rowSpan cell @?= 2

-- Test parsing second header row directly  
testParseSecondHeaderRow :: Assertion
testParseSecondHeaderRow = do
    let tags = parseTags secondHeaderRowHTML
        headerCells = parseHeaderRow tags
        expectedCount = 11  -- 11 individual column headers
        expectedHeaders = ["Field Production", "Transfers to Crude Oil Supply", "Biofuels Plant Net Production",
                          "Refinery & Blender Net Production", "Imports", "Net Receipts", "Adjust- ments",
                          "Stock Change", "Refinery & Blender Net Inputs", "Exports", "Products Supplied"]
    length headerCells @?= expectedCount
    let actualHeaders = map headerText headerCells
    actualHeaders @?= expectedHeaders

-- Test resolving two-row header matrix to final headers structure
testResolveHeaderMatrix :: Assertion
testResolveHeaderMatrix = do
    let firstRowTags = parseTags firstHeaderRowHTML
        secondRowTags = parseTags secondHeaderRowHTML
        maybeMatrix = buildHeaderMatrix [firstRowTags, secondRowTags]
    case maybeMatrix of
        Nothing -> assertFailure "Could not build header matrix"
        Just matrix -> do
            -- Matrix should have: [Area navigation] + [11 individual headers] + [Ending Stocks] = 13 total
            let allHeaders = resolveHeaderList matrix 13
                expectedDataHeaders = ["Field Production", "Transfers to Crude Oil Supply", "Biofuels Plant Net Production",
                                      "Refinery & Blender Net Production", "Imports", "Net Receipts", "Adjust- ments", 
                                      "Stock Change", "Refinery & Blender Net Inputs", "Exports", "Products Supplied"]
                actualDataHeaders = take 11 (drop 1 allHeaders)  -- Skip area column, take 11 data headers
                lastHeader = last allHeaders
            length allHeaders @?= 13
            actualDataHeaders @?= expectedDataHeaders
            lastHeader @?= "Ending Stocks"

-- Debug what headers are actually extracted from full page
testFullPageHeaders :: BL.ByteString -> Assertion
testFullPageHeaders html = do
    let tags = parseTags (unpack html)
        headers = extractEiaHeaders tags
        expectedCount = 12
    putStrLn $ "=== FULL PAGE DEBUG: Headers ==="
    putStrLn $ "Header count: " ++ show (length headers)
    putStrLn $ "Headers: " ++ show headers
    length headers @?= expectedCount

-- Debug what rows are actually extracted from full page  
testFullPageRows :: BL.ByteString -> Assertion
testFullPageRows html = do
    let tags = parseTags (unpack html)
        rows = extractEiaRows tags
        expectedCount = 6
    putStrLn $ "=== FULL PAGE DEBUG: Rows ==="
    putStrLn $ "Row count: " ++ show (length rows)
    mapM_ (\(i, row) -> putStrLn $ "Row " ++ show i ++ ": " ++ show (take 3 row ++ ["..."])) (zip [0..] rows)
    length rows @?= expectedCount

-- Debug the specific structure of PADD 1 row
testPadd1RowStructure :: BL.ByteString -> Assertion  
testPadd1RowStructure html = do
    let tags = parseTags (unpack html)
        rows = extractEiaRows tags
        padd1Row = find (\row -> case row of
                                   [] -> False
                                   (firstCell:_) -> "PADD 1" == firstCell || "PADD1" == firstCell) rows
    putStrLn $ "=== FULL PAGE DEBUG: PADD 1 Row ==="
    case padd1Row of
        Nothing -> do
            putStrLn "PADD 1 row not found!"
            let allFirstCells = map (\row -> case row of [] -> ""; (x:_) -> x) rows
            putStrLn $ "Available first cells: " ++ show allFirstCells
            assertFailure "PADD 1 row not found"
        Just row -> do
            putStrLn $ "PADD 1 row length: " ++ show (length row)
            putStrLn $ "PADD 1 row: " ++ show row
            length row @?= 13  -- Should have area + 12 data columns

-- Debug column index matching
testColumnIndexMatching :: BL.ByteString -> Assertion
testColumnIndexMatching html = do
    let tags = parseTags (unpack html)
        headers = extractEiaHeaders tags
        endingStocksIndex = findColumnIndex headers "Ending Stocks"
        exportsIndex = findColumnIndex headers "Exports"
        stockChangeIndex = findColumnIndex headers "Stock Change"
    putStrLn $ "=== FULL PAGE DEBUG: Column Indices ==="
    putStrLn $ "Headers: " ++ show headers
    putStrLn $ "Ending Stocks index: " ++ show endingStocksIndex
    putStrLn $ "Exports index: " ++ show exportsIndex  
    putStrLn $ "Stock Change index: " ++ show stockChangeIndex
    -- These should all find indices
    assertBool "Should find Ending Stocks index" (isJust endingStocksIndex)
    assertBool "Should find Exports index" (isJust exportsIndex)
    assertBool "Should find Stock Change index" (isJust stockChangeIndex)

-- Debug the specific structure of U.S. row
testUsRowStructure :: BL.ByteString -> Assertion
testUsRowStructure html = do
    let tags = parseTags (unpack html)
        -- Apply the same HTML fix that extractTable uses  
        htmlStr = unpack html
        fixedHtmlStr = fixMalformedHtml htmlStr
        fixedTags = parseTags fixedHtmlStr
        rows = extractEiaRows fixedTags
        usRow = find (\row -> case row of
                                [] -> False
                                (firstCell:_) -> "U.S." == firstCell) rows
    putStrLn $ "=== FULL PAGE DEBUG: U.S. Row ==="
    case usRow of
        Nothing -> do
            putStrLn "U.S. row not found!"
            let allFirstCells = map (\row -> case row of [] -> ""; (x:_) -> x) rows
            putStrLn $ "Available first cells: " ++ show allFirstCells
            putStrLn "Looking at first row in detail..."
            if not (null rows) then do
                let firstRow = head rows
                putStrLn $ "First row length: " ++ show (length firstRow)
                putStrLn $ "First row content: " ++ show firstRow
                putStrLn $ "First row's first 3 cells: " ++ show (take 3 firstRow)
                putStrLn "Looking for U.S. in actual HTML..."
                let htmlContent = unpack html
                    usMatches = filter (isInfixOf "U.S.") $ lines htmlContent
                putStrLn $ "Lines containing 'U.S.': " ++ show (length usMatches)
                forM_ (take 3 usMatches) $ \line -> 
                    putStrLn $ "  " ++ take 100 line
                putStrLn "Testing HTML fix..."
                let testFixedTags = parseTags (fixMalformedHtml htmlContent)
                    fixedRows = extractEiaRows testFixedTags
                putStrLn $ "Rows from fixed HTML: " ++ show (length fixedRows)
                if not (null fixedRows) then do
                    let firstFixedRow = head fixedRows
                    putStrLn $ "First fixed row length: " ++ show (length firstFixedRow)
                    putStrLn $ "First fixed row content: " ++ show (take 3 firstFixedRow)
                else 
                    putStrLn "No rows found even after HTML fix!"
            else do
                putStrLn "No rows found at all!"
            assertFailure "U.S. row not found"
        Just row -> do
            putStrLn $ "U.S. row length: " ++ show (length row)
            putStrLn $ "U.S. row: " ++ show row
            length row @?= 13  -- Should have area + 12 data columns