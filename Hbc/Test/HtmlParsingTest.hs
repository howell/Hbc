{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main where

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (unpack, pack)
import Text.HTML.TagSoup (Tag(..), parseTags, sections, (~==), (~/=), innerText)
import Data.List (find, maximumBy, isInfixOf)
import Data.Maybe (listToMaybe, catMaybes)
import Data.Char (isSpace, toLower)
import Control.Monad (when)
import Text.Read (readMaybe)

-- Import the actual parsing functions from Monthly module
import Hbc.Monthly (parseHtmlTable, extractTable, extractHeaderRow, extractDataRows, 
                    findRowByName, findColumnIndex, extractCellData, normalizeText,
                    extractEiaHeaders, extractEiaRows, extractEiaRowLabel, extractEiaDataCells,
                    parseTableStructure, findEiaDataTable, findHeaderRows, countDataColumns, buildHeaderMatrix,
                    TableStructure(..), HeaderCell(..))

main :: IO ()
main = do
    putStrLn "=== EIA HTML Table Parsing Test Suite ==="
    html <- BL.readFile "eia_sample_page.html"
    runTests html

runTests :: BL.ByteString -> IO ()
runTests html = do
    -- putStrLn "\n1. Testing basic HTML parsing..."
    -- testBasicParsing html
    
    -- putStrLn "\n2. Testing table extraction..."
    -- testTableExtraction html
    
    -- putStrLn "\n3. Testing specific data point extraction..."
    -- testDataPointExtraction html
    
    -- putStrLn "\n4. Testing parseHtmlTable function..."
    -- testParseHtmlTable html
    
    -- putStrLn "\n5. Debugging row extraction..."
    -- debugRowExtraction html
    
    putStrLn "\n6. Debugging structure-based table parsing..."
    debugTableStructure html
    
    putStrLn "\n7. Debugging actual data extraction..."
    debugActualExtraction html
    
    putStrLn "\n8. Debugging direct TD extraction..."
    debugDirectTDExtraction html

testBasicParsing :: BL.ByteString -> IO ()
testBasicParsing html = do
    let tags = parseTags (unpack html)
    putStrLn $ "Total HTML tags: " ++ show (length tags)
    
    let tableCount = length $ filter (~== ("<table>" :: String)) tags
    putStrLn $ "Tables found: " ++ show tableCount
    
    let headerRows = filter (~== ("<th>" :: String)) tags
    putStrLn $ "Header cells found: " ++ show (length headerRows)

testTableExtraction :: BL.ByteString -> IO ()
testTableExtraction html = do
    let tags = parseTags (unpack html)
        tableResult = extractTable tags
    
    case tableResult of
        Nothing -> putStrLn "FAILED: Could not extract table"
        Just (headers, rows) -> do
            putStrLn $ "SUCCESS: Table extracted"
            putStrLn $ "Headers (" ++ show (length headers) ++ "): " ++ show headers
            putStrLn $ "Rows found: " ++ show (length rows)
            
            -- Show first few rows
            putStrLn "First 3 rows:"
            mapM_ (\(i, row) -> putStrLn $ "  Row " ++ show i ++ ": " ++ show (take 3 row)) 
                  (zip [0..2] (take 3 rows))

testDataPointExtraction :: BL.ByteString -> IO ()
testDataPointExtraction html = do
    putStrLn "Testing data extraction with parseHtmlTable..."
    let testCases = [("U.S.", "Field Production"), 
                     ("PADD 1", "Imports"), 
                     ("PADD 2", "Exports"),
                     ("U.S.", "Imports")]
    mapM_ (testSingleDataPoint html) testCases

testSingleDataPoint :: BL.ByteString -> (String, String) -> IO ()
testSingleDataPoint html (rowName, colName) = do
    let result = parseHtmlTable html rowName colName
    case result of
        Nothing -> putStrLn $ "  FAILED: " ++ rowName ++ " / " ++ colName
        Just val -> putStrLn $ "  SUCCESS: " ++ rowName ++ " / " ++ colName ++ " = " ++ unpack val

testParseHtmlTable :: BL.ByteString -> IO ()
testParseHtmlTable html = do
    putStrLn "Testing individual parsing components..."
    
    let tags = parseTags (unpack html)
    
    -- Test header extraction
    case extractTable tags of
        Nothing -> putStrLn "  extractTable FAILED"
        Just (headers, rows) -> do
            putStrLn $ "  extractTable SUCCESS: " ++ show (length headers) ++ " headers, " ++ show (length rows) ++ " rows"
            
            -- Test row finding
            let testRow = findRowByName rows "U.S."
            case testRow of
                Nothing -> putStrLn "  findRowByName FAILED for 'U.S.'"
                Just row -> putStrLn $ "  findRowByName SUCCESS: U.S. row has " ++ show (length row) ++ " cells"
            
            -- Test column finding
            let testCol = findColumnIndex headers "Field Production"
            case testCol of
                Nothing -> putStrLn "  findColumnIndex FAILED for 'Field Production'"
                Just idx -> putStrLn $ "  findColumnIndex SUCCESS: 'Field Production' at index " ++ show idx

debugRowExtraction :: BL.ByteString -> IO ()
debugRowExtraction html = do
    let tags = parseTags (unpack html)
        rows = extractEiaRows tags
    
    putStrLn $ "DEBUG: Found " ++ show (length rows) ++ " rows"
    
    -- Show first element of each row (should be row name)
    mapM_ (\(i, row) -> do
        case row of
            [] -> putStrLn $ "  Row " ++ show i ++ ": EMPTY"
            (name:cells) -> do
                putStrLn $ "  Row " ++ show i ++ " name: '" ++ name ++ "'"
                putStrLn $ "  Row " ++ show i ++ " cells: " ++ show (take 5 cells)
        ) (zip [0..] rows)
    
    -- Test row matching
    putStrLn "\nTesting row matching:"
    let testRowNames = ["U.S.", "PADD 1", "PADD 2"]
    mapM_ (\rowName -> do
        let match = findRowByName rows rowName
        case match of
            Nothing -> putStrLn $ "  '" ++ rowName ++ "' NOT FOUND"
            Just foundRow -> putStrLn $ "  '" ++ rowName ++ "' FOUND: " ++ show (take 3 foundRow)
        ) testRowNames
        
    -- Debug data cell extraction for first row
    putStrLn "\nDebugging data cell extraction for first row:"
    let dataRows = sections (~== ("<tr class=\"DataRow\">" :: String)) tags
    case dataRows of
        [] -> putStrLn "  No data rows found!"
        (firstRow:_) -> do
            let scopedRow = Prelude.takeWhile (~/= ("</tr>" :: String)) firstRow
                dataCells = extractEiaDataCells scopedRow
            putStrLn $ "  Raw row tags count: " ++ show (length firstRow)  
            putStrLn $ "  Scoped row tags count: " ++ show (length scopedRow)
            putStrLn $ "  Data cells found: " ++ show (length dataCells)
            putStrLn $ "  Data cells: " ++ show dataCells
            
            -- Count TD elements manually
            let tdElements = filter (~== ("<TD" :: String)) scopedRow
                tdClassDataA = filter (~== ("<TD class=DataA" :: String)) scopedRow  
                tdClassDataS = filter (~== ("<TD class=DataS" :: String)) scopedRow
            putStrLn $ "  TD elements found: " ++ show (length tdElements)
            putStrLn $ "  TD class=DataA found: " ++ show (length tdClassDataA)
            putStrLn $ "  TD class=DataS found: " ++ show (length tdClassDataS)

-- Debug function to analyze table structure parsing issues
debugTableStructure :: BL.ByteString -> IO ()
debugTableStructure html = do
    let tags = parseTags (unpack html)
    
    putStrLn "=== DEBUG TABLE STRUCTURE ==="
    
    -- Debug EIA table scoping
    case findEiaDataTable tags of
        Nothing -> putStrLn "No EIA data table found!"
        Just eiaTableTags -> do
            putStrLn $ "Found EIA data table with " ++ show (length eiaTableTags) ++ " tags"
            let dataRows = filter (~== ("<tr class=\"DataRow\">" :: String)) eiaTableTags
            putStrLn $ "EIA table contains " ++ show (length dataRows) ++ " DataRow elements"
            
            -- Debug header rows within EIA table scope
            case findHeaderRows eiaTableTags of
                Nothing -> putStrLn "No header rows found in EIA table"
                Just headerRows -> do
                    putStrLn $ "Found " ++ show (length headerRows) ++ " header rows in EIA table"
                    mapM_ (\(i, row) -> do
                        let thCount = length $ filter (~== ("<th" :: String)) row
                        putStrLn $ "Header row " ++ show i ++ " TH count: " ++ show thCount
                        ) (zip [0..] headerRows)
            
            -- Debug data columns within EIA table scope
            case countDataColumns eiaTableTags of
                Nothing -> putStrLn "No data columns found in EIA table"
                Just dataColCount -> putStrLn $ "Data columns in EIA table: " ++ show dataColCount
            
            -- Debug DataRow analysis
            let allTRs = sections (~== ("<tr" :: String)) eiaTableTags
                dataRowTRs = filter isDataRowTR allTRs
                isDataRowTR tr = case tr of
                    [] -> False
                    (firstTag:_) -> firstTag ~== ("<tr class=\"DataRow\">" :: String)
            putStrLn $ "Total TRs in EIA table: " ++ show (length allTRs)
            putStrLn $ "DataRow TRs found: " ++ show (length dataRowTRs)
            
            -- Analyze first DataRow in detail
            case dataRowTRs of
                [] -> putStrLn "No DataRow TRs found!"
                (firstDataRowTR:_) -> do
                    let scopedRow = firstDataRowTR  -- Use full DataRow, don't cut at first </tr>
                        allTDs = filter (~== ("<td" :: String)) scopedRow
                        allTHs = filter (~== ("<th" :: String)) scopedRow
                        allTDsLoose = filter (\tag -> case tag of 
                                                TagOpen tagName _ -> tagName == "td"
                                                _ -> False) scopedRow
                    putStrLn $ "First DataRow - Total tags: " ++ show (length firstDataRowTR)
                    putStrLn $ "First DataRow - Scoped tags: " ++ show (length scopedRow)
                    putStrLn $ "First DataRow - TD count: " ++ show (length allTDs)
                    putStrLn $ "First DataRow - TH count: " ++ show (length allTHs)
                    putStrLn $ "First DataRow - TD count (loose): " ++ show (length allTDsLoose)
                    putStrLn $ "First 10 tags in DataRow:"
                    mapM_ (putStrLn . ("  " ++) . show) (take 10 scopedRow)
                    
                    -- Debug nested table structure 
                    let nestedTables = sections (~== ("<table" :: String)) scopedRow
                    putStrLn $ "Found " ++ show (length nestedTables) ++ " nested tables in DataRow"
                    -- Analyze all nested tables to find the data table
                    mapM_ (\(tableIdx, table) -> do
                        putStrLn $ "Nested table " ++ show tableIdx ++ ": " ++ show (length table) ++ " tags"
                        case table of
                            (TagOpen "table" attrs : _) -> 
                                putStrLn $ "  Attributes: " ++ show attrs
                            _ -> putStrLn "  No opening tag found"
                        let tableTRs = sections (~== ("<tr" :: String)) table
                        putStrLn $ "  " ++ show (length tableTRs) ++ " TR elements"
                        mapM_ (\(trIdx, tr) -> do
                            let tdCount = length $ filter (~== ("<td" :: String)) tr
                            putStrLn $ "    TR " ++ show trIdx ++ ": " ++ show tdCount ++ " TDs"
                            when (tdCount > 5) $ do  -- Show details for interesting rows
                                putStrLn $ "      First 8 tags: " ++ show (take 8 tr)
                            ) (zip [0..] (take 3 tableTRs))
                        ) (zip [0..] nestedTables)
    
    -- Debug full table structure parsing
    case parseTableStructure tags of
        Nothing -> putStrLn "Failed to parse table structure"
        Just tableStructure -> do
            putStrLn $ "Matrix rows: " ++ show (length (headerMatrix tableStructure))
            mapM_ (\(i, row) -> do
                putStrLn $ "Matrix row " ++ show i ++ " length: " ++ show (length row)
                ) (zip [0..] (headerMatrix tableStructure))
            putStrLn $ "Data column count: " ++ show (dataColumnCount tableStructure)
            putStrLn $ "Final headers (" ++ show (length (finalHeaders tableStructure)) ++ "): " ++ show (finalHeaders tableStructure)
            
    -- Debug column filtering - show what we're including vs excluding
    putStrLn "\n=== COLUMN FILTERING DEBUG ==="
    case findEiaDataTable tags of
        Nothing -> putStrLn "No EIA data table found for column debug!"
        Just eiaTableTags -> debugColumnFiltering eiaTableTags

-- Debug column filtering to see what we include vs exclude
debugColumnFiltering :: [Tag String] -> IO ()
debugColumnFiltering eiaTableTags = do
    let allTRs = sections (~== ("<tr" :: String)) eiaTableTags
        dataRowTRs = filter isDataRowTR allTRs
        isDataRowTR tr = case tr of
            [] -> False
            (firstTag:_) -> firstTag ~== ("<tr class=\"DataRow\">" :: String)
    case dataRowTRs of
        [] -> putStrLn "No DataRow TRs found for column debug!"
        (firstDataRowTR:_) -> do
            let nestedTables = sections (~== ("<table" :: String)) firstDataRowTR
                dataTables = filter isDataTable nestedTables
                mainTable = case dataTables of
                    [] -> Nothing
                    tables -> Just $ maximumBy (\t1 t2 -> compare (length t1) (length t2)) tables
            case mainTable of
                Nothing -> putStrLn "No main nested table found for column debug!"
                Just table -> do
                    let tableTRs = sections (~== ("<tr" :: String)) table
                        firstTR = listToMaybe tableTRs
                    case firstTR of
                        Nothing -> putStrLn "No first TR found in nested table!"
                        Just tr -> do
                            let tdSections = sections (~== ("<td" :: String)) tr
                            putStrLn $ "Found " ++ show (length tdSections) ++ " total TD sections in first TR"
                            
                            -- Analyze each TD section
                            mapM_ (\(i, tdSection) -> do
                                case tdSection of
                                    [] -> putStrLn $ "TD " ++ show i ++ ": EMPTY"
                                    (TagOpen "td" attrs : rest) -> do
                                        let widthAttr = lookup "width" attrs
                                            classAttr = lookup "class" attrs
                                            isData = isDataColumn tdSection
                                            cellContent = take 3 $ filter isTextTag rest
                                        putStrLn $ "TD " ++ show i ++ ": width=" ++ show widthAttr ++ 
                                                   " class=" ++ show classAttr ++ 
                                                   " isData=" ++ show isData ++
                                                   " content=" ++ show cellContent
                                    _ -> putStrLn $ "TD " ++ show i ++ ": Non-standard format"
                                ) (zip [0..] tdSections)
                            
                            let dataColumns = filter isDataColumn tdSections
                            putStrLn $ "\nFiltered to " ++ show (length dataColumns) ++ " data columns"
  where
    isDataTable tableSection = 
        case tableSection of
            [] -> False
            (TagOpen "table" attrs : _) -> 
                case lookup "class" attrs of
                    Just className -> className == "data2"
                    Nothing -> False
            _ -> False
    
    isDataColumn tdSection =
        case tdSection of
            [] -> False
            (TagOpen "td" attrs : _) -> 
                case (lookup "width" attrs, lookup "class" attrs) of
                    (Just widthStr, _) -> 
                        case readMaybe widthStr of
                            Just width -> width > 10 && not (hasClass "DataStub" attrs)
                            Nothing -> not (hasClass "DataStub" attrs)
                    (Nothing, _) -> not (hasClass "DataStub" attrs)
            _ -> False
    
    hasClass targetClass attrs = 
        case lookup "class" attrs of
            Just className -> isInfixOf targetClass className
            Nothing -> False
            
    isTextTag (TagText _) = True
    isTextTag _ = False

-- Debug function to show what we're actually extracting
debugActualExtraction :: BL.ByteString -> IO ()
debugActualExtraction html = do
    let tags = parseTags (unpack html)
    
    case extractTable tags of
        Nothing -> putStrLn "No table extracted!"
        Just (headers, rows) -> do
            putStrLn $ "Headers (" ++ show (length headers) ++ "): " ++ show headers
            putStrLn "\nActual extracted data:"
            mapM_ debugRowWithValues (zip [0..] rows)
            
            -- Test specific problematic cases
            putStrLn "\n=== SPECIFIC EXTRACTION TESTS ==="
            let testCases = [ ("U.S.", "Stock Change")
                            , ("U.S.", "Exports") 
                            , ("PADD 1", "Adjust- ments")
                            , ("PADD 1", "Ending Stocks")
                            , ("PADD 1", "Imports")
                            ]
            mapM_ (testSpecificExtraction html) testCases

debugRowWithValues :: (Int, [String]) -> IO ()
debugRowWithValues (i, row) = 
    case row of
        (rowName:values) -> do
            putStrLn $ "Row " ++ show i ++ " (" ++ rowName ++ "):"
            putStrLn $ "  Values (" ++ show (length values) ++ "): " ++ show values
        _ -> putStrLn $ "Row " ++ show i ++ ": malformed - " ++ show row

testSpecificExtraction :: BL.ByteString -> (String, String) -> IO ()
testSpecificExtraction html (rowName, colName) = do
    let result = parseHtmlTable html rowName colName
    putStrLn $ "  " ++ rowName ++ " / " ++ colName ++ ": " ++ show (fmap unpack result)

-- Debug direct TD extraction to understand what's happening
debugDirectTDExtraction :: BL.ByteString -> IO ()
debugDirectTDExtraction html = do
    let tags = parseTags (unpack html)
    
    putStrLn "=== DEBUG DIRECT TD EXTRACTION ==="
    
    -- Find first DataRow and examine its TRs
    case findEiaDataTable tags of
        Nothing -> putStrLn "No EIA data table found!"
        Just eiaTableTags -> do
            let dataRows = sections (~== ("<tr class=\"DataRow\">" :: String)) eiaTableTags
            putStrLn $ "Found " ++ show (length dataRows) ++ " DataRow elements"
            
            case dataRows of
                [] -> putStrLn "No DataRow elements found!"
                (firstDataRow:_) -> do
                    putStrLn "\nAnalyzing first DataRow:"
                    let allTRs = sections (~== ("<tr" :: String)) firstDataRow
                    putStrLn $ "Found " ++ show (length allTRs) ++ " TR elements in first DataRow"
                    
                    -- Check each TR for DataA/DataS TDs
                    mapM_ (analyzeTRForDataTDs 0) (take 5 allTRs)

analyzeTRForDataTDs :: Int -> [Tag String] -> IO ()
analyzeTRForDataTDs trIndex tr = do
    let allTDs = sections (~== ("<td" :: String)) tr ++ sections (~== ("<TD" :: String)) tr
        dataATDs = filter hasDataAClass allTDs
        dataSCells = filter hasDataSClass allTDs
        -- Check if this TR contains nested tables (which would indicate it's not the simple data row)
        nestedTables = sections (~== ("<table" :: String)) tr
        hasNested = length nestedTables > 0
        -- Test the new consecutive TD detection
        consecutiveTDs = findConsecutiveTDsDebug tr 0 []
    putStrLn $ "TR " ++ show trIndex ++ ": " ++ show (length allTDs) ++ " total TDs, " ++ 
               show (length dataATDs) ++ " DataA, " ++ show (length dataSCells) ++ " DataS, " ++
               show (length consecutiveTDs) ++ " consecutive TDs" ++
               (if hasNested then " (has nested tables)" else " (simple TR)")
    when (length dataATDs + length dataSCells >= 8) $ do
        putStrLn $ "  *** This TR has " ++ show (length dataATDs + length dataSCells) ++ " data TDs - potential candidate!"
  where
    hasDataAClass tdSection =
        case tdSection of
            [] -> False
            (TagOpen _ attrs : _) -> 
                case lookup "class" attrs of
                    Just className -> "DataA" `isInfixOf` className
                    Nothing -> False
            _ -> False
    
    hasDataSClass tdSection =
        case tdSection of
            [] -> False
            (TagOpen _ attrs : _) -> 
                case lookup "class" attrs of
                    Just className -> "DataS" `isInfixOf` className
                    Nothing -> False
            _ -> False
    
    -- Debug version of findConsecutiveTDs
    findConsecutiveTDsDebug :: [Tag String] -> Int -> [[Tag String]] -> [[Tag String]]
    findConsecutiveTDsDebug [] _ acc = reverse acc
    findConsecutiveTDsDebug (tag:rest) depth acc = 
        case tag of
            -- Found any TD at depth 0 - extract it
            TagOpen tagName _ | depth == 0 && map toLower tagName == "td" ->
                let (tdContent, remaining) = extractTDSectionDebug (tag:rest) 0
                in findConsecutiveTDsDebug remaining depth (tdContent:acc)
            -- Entering nested element - increase depth
            TagOpen _ _ -> findConsecutiveTDsDebug rest (depth + 1) acc
            -- Exiting nested element - decrease depth
            TagClose _ -> findConsecutiveTDsDebug rest (max 0 (depth - 1)) acc
            -- Other content - continue
            _ -> findConsecutiveTDsDebug rest depth acc
    
    -- Debug version of extractTDSection
    extractTDSectionDebug :: [Tag String] -> Int -> ([Tag String], [Tag String])
    extractTDSectionDebug [] _ = ([], [])
    extractTDSectionDebug (tag:rest) depth = 
        case tag of
            TagOpen "td" _ | depth == 0 -> 
                let (content, remaining) = extractTDSectionDebug rest 1
                in (tag:content, remaining)
            TagClose "td" | depth == 1 -> ([tag], rest)
            TagOpen _ _ -> 
                let (content, remaining) = extractTDSectionDebug rest (depth + 1)
                in (tag:content, remaining)
            TagClose _ -> 
                let (content, remaining) = extractTDSectionDebug rest (depth - 1)
                in (tag:content, remaining)
            _ -> 
                let (content, remaining) = extractTDSectionDebug rest depth
                in (tag:content, remaining)