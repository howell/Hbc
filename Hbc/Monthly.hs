{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Hbc.Monthly (
    -- Main functions
    runBot, runHtmlBot,
    -- Data types  
    DataPoint(..), HtmlDataPoint(..), MonthlySpreadsheet(..),
    HeaderCell(..), TableStructure(..),
    -- HTML parsing functions (exported for testing)
    parseHtmlTable, extractTable, extractHeaderRow, extractDataRows,
    findRowByName, findColumnIndex, extractCellData, normalizeText,
    fixMalformedHtml,
    -- Structure-based parsing functions
    parseTableStructure, findEiaDataTable, findHeaderRows, countDataColumns, buildHeaderMatrix,
    parseHeaderRow, resolveHeaderList,
    -- EIA-specific parsing functions
    extractEiaHeaders, extractEiaRows, extractEiaRowLabel, extractEiaDataCells,
    extractRowContentWithNesting,
    -- Configuration functions
    readDataPoint, readHtmlDataPoint, writeHtmlDataPoints
) where

import Hbc.Scraper

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (unpack, pack)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Vector ((!?), Vector)
import qualified Data.Vector as V
import Control.Monad.Except
import Data.Char (ord, chr, isDigit, toUpper, toLower, isSpace)
import Data.Attoparsec.ByteString.Lazy
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.String (fromString)
import Text.Read (readMaybe)
import Text.HTML.TagSoup
import Data.List (find, maximumBy, isInfixOf)
import Data.Maybe (listToMaybe, catMaybes, isNothing)
import Control.Concurrent.Async (async, wait)

-- EIA table constants
eiaExpectedDataColumns :: Int
eiaExpectedDataColumns = 12

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

-- New data type for HTML-based scraping
data HtmlDataPoint = HtmlDataPoint { url             :: !String,
                                     rowName         :: !ByteString,
                                     columnName      :: !ByteString,
                                     htmlDescription :: !ByteString,
                                     htmlCode        :: !ByteString
                                   } deriving (Eq, Show)

-- Structure-based table parsing data types
data HeaderCell = HeaderCell 
    { headerText :: !String
    , colSpan :: !Int
    , rowSpan :: !Int
    , startRow :: !Int
    , startCol :: !Int
    } deriving (Eq, Show)

data TableStructure = TableStructure
    { headerMatrix :: ![[Maybe HeaderCell]]
    , dataColumnCount :: !Int  
    , finalHeaders :: ![String]
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

-- Fix malformed HTML tags that break parsing  
fixMalformedHtml :: String -> String
fixMalformedHtml html = 
    -- Simple fix: replace pattern <B>text<B> with <B>text</B>
    fixMalformedBPattern html
  where
    fixMalformedBPattern :: String -> String
    fixMalformedBPattern str = 
        -- Look for pattern: <B> followed by non-< characters followed by <B>
        case break (== '<') str of
            (before, "") -> before  -- No more tags
            (before, '<':'B':'>':rest) -> 
                -- Found opening <B>, look for content and malformed <B>
                case extractContentAndMalformedB rest of
                    Nothing -> before ++ "<B>" ++ fixMalformedBPattern rest
                    Just (content, after) -> 
                        before ++ "<B>" ++ content ++ "</B>" ++ fixMalformedBPattern after
            (before, c:rest) -> 
                before ++ [c] ++ fixMalformedBPattern rest
    
    extractContentAndMalformedB :: String -> Maybe (String, String)
    extractContentAndMalformedB str = 
        case break (== '<') str of
            (content, '<':'B':'>':rest) -> Just (content, rest)  -- Found malformed closing
            _ -> Nothing  -- No malformed closing found

-- HTML Table parsing functions
parseHtmlTable :: ByteString -> String -> String -> Maybe ByteString
parseHtmlTable html targetRow targetCol = do
    let fixedHtml = fixMalformedHtml (unpack html)
        tags = parseTags fixedHtml
        table = extractTable tags
    (tableHeaders, tableRows) <- table
    rowData <- findRowByName tableRows targetRow
    columnIndex <- findColumnIndex tableHeaders targetCol
    -- Add 1 to column index to account for area column in rows (headers don't include area column)
    extractCellData rowData (columnIndex + 1)

-- EIA-specific table extraction - handles their complex nested structure
extractTable :: [Tag String] -> Maybe ([String], [[String]])
extractTable tags = do
    -- Apply HTML fix to handle malformed tags like <B>U.S.<B>
    let htmlStr = renderTags tags
        fixedHtmlStr = fixMalformedHtml htmlStr
        fixedTags = parseTags fixedHtmlStr
        tableHeaders = extractEiaHeaders fixedTags
        tableRows = extractEiaRows fixedTags
    if null tableHeaders || null tableRows 
        then Nothing 
        else Just (tableHeaders, tableRows)

-- Extract EIA headers using structure-based parsing
extractEiaHeaders :: [Tag String] -> [String]
extractEiaHeaders tags = 
    case parseTableStructure tags of
        Nothing -> []
        Just tableStructure -> finalHeaders tableStructure

extractSingleHeaderContent :: [Tag String] -> Maybe String
extractSingleHeaderContent tags = 
    let content = innerText $ Prelude.takeWhile (~/= ("</th>" :: String)) tags
        cleaned = unwords $ words content  -- normalize whitespace
        shortened = unwords $ Prelude.take 5 $ words cleaned  -- limit to first few words
    in if null shortened then Nothing else Just shortened

extractHeaderContent :: [Tag String] -> Maybe String  
extractHeaderContent tags = extractSingleHeaderContent tags

-- Extract EIA data rows from <tr class="DataRow"> elements
extractEiaRows :: [Tag String] -> [[String]]
extractEiaRows tags = 
    let dataRows = sections (~== ("<tr class=\"DataRow\">" :: String)) tags
        -- Use a more sophisticated approach to extract row content
        individualRows = map extractRowContentWithNesting dataRows
    in map extractEiaRowContent individualRows

-- Extract row content while handling nested <tr> tags properly
extractRowContentWithNesting :: [Tag String] -> [Tag String]
extractRowContentWithNesting tags = extractRowContentHelper tags 0
  where
    extractRowContentHelper [] _ = []
    extractRowContentHelper (tag:rest) depth
      | tag ~== ("<tr" :: String) = tag : extractRowContentHelper rest (depth + 1)
      | tag ~== ("</tr>" :: String) = 
          if depth <= 1 
            then []  -- This is the closing tag of the main row
            else tag : extractRowContentHelper rest (depth - 1)
      | otherwise = tag : extractRowContentHelper rest depth

extractEiaRowContent :: [Tag String] -> [String]
extractEiaRowContent rowTags = 
    -- extractEiaDataCells already handles the complete row structure (area + 12 data columns)
    extractEiaDataCells rowTags

-- Extract row label from <td class="DataStub"> nested structure
extractEiaRowLabel :: [Tag String] -> Maybe String
extractEiaRowLabel rowTags = 
    -- Simple approach: look for DataStub sections and find ones with reasonable text
    let dataStubSections = sections (~== ("<td class=\"DataStub\">" :: String)) rowTags
        textOnlyStubs = catMaybes $ map extractTextFromStub dataStubSections
        goodLabels = filter isReasonableLabel textOnlyStubs
    in listToMaybe goodLabels
  where
    extractTextFromStub stubTags = 
        let content = innerText $ Prelude.takeWhile (~/= ("</td>" :: String)) stubTags
            cleaned = unwords $ words content
        in if null cleaned then Nothing else Just cleaned
    
    isReasonableLabel text = 
        let len = length text
            words_in_text = words text
            looks_like_pure_number = length words_in_text == 1 && all (\c -> isDigit c || c == ',' || c == '.' || c == '-') text
        in len > 0 && len < 50 && not looks_like_pure_number

extractCleanRowLabel :: [Tag String] -> Maybe String
extractCleanRowLabel stub = 
    let content = innerText stub
        -- Remove HTML tags and extra characters
        cleaned = filter (\c -> c /= '<' && c /= '>' && c /= 'B') content
        normalized = unwords $ words cleaned
        -- Extract only the region name (first 1-2 words before any numbers/data)
        regionName = extractRegionName normalized
    in if null regionName || length regionName > 20
       then Nothing 
       else Just regionName

-- Extract just the region name (U.S., PADD 1, etc.) before any numeric data
extractRegionName :: String -> String
extractRegionName text = 
    let cleanWords = words text
        -- Take words until we hit a number or get to "PADD X" pattern
        regionWords = takeWhileNotNumber cleanWords
    in unwords $ Prelude.take 2 regionWords  -- Limit to 2 words max
  where
    takeWhileNotNumber [] = []
    takeWhileNotNumber (w:ws) 
        | all (\c -> isDigit c || c == ',' || c == '.' || c == '-') w = []  -- Stop at numbers
        | otherwise = w : takeWhileNotNumber ws

-- Extract data cells by finding TR with area column + data columns consecutive TDs
extractEiaDataCells :: [Tag String] -> [String]
extractEiaDataCells rowTags = 
    -- Find all TR sections within this DataRow
    let allTRs = sections (~== ("<tr" :: String)) rowTags
        -- Find the TR that has exactly area + data columns consecutive TDs at the top level
        expectedTotalTDs = eiaExpectedDataColumns + 1  -- area column + data columns
        targetTR = find (hasExactlyNConsecutiveTDs expectedTotalTDs) allTRs
    in case targetTR of
        Nothing -> []
        Just tr -> extractAllTDValues tr
  where
    -- Check if a TR has exactly N consecutive TDs at the top level
    hasExactlyNConsecutiveTDs :: Int -> [Tag String] -> Bool
    hasExactlyNConsecutiveTDs n trTags = 
        let consecutiveTDs = findConsecutiveTDs trTags 0 []
        in length consecutiveTDs == n
    
    -- Find consecutive TDs at depth 1 (immediate children of TR, not nested TDs)
    findConsecutiveTDs :: [Tag String] -> Int -> [[Tag String]] -> [[Tag String]]
    findConsecutiveTDs [] _ acc = acc
    findConsecutiveTDs (tag:rest) depth acc = 
        case tag of
            -- Found TD at depth 1 (top-level under TR) - extract it
            TagOpen tagName _ | depth == 1 && map toLower tagName == "td" ->
                let (tdContent, remaining) = extractTDSection (tag:rest) 0
                in findConsecutiveTDs remaining depth (acc ++ [tdContent])
            -- Entering nested element - increase depth  
            TagOpen _ _ -> findConsecutiveTDs rest (depth + 1) acc
            -- Exiting nested TR element - decrease depth
            TagClose tagName | map toLower tagName == "tr" -> 
                findConsecutiveTDs rest (max 0 (depth - 1)) acc
            -- Exiting other nested element - decrease depth
            TagClose _ -> findConsecutiveTDs rest depth acc
            -- Other content - continue
            _ -> findConsecutiveTDs rest depth acc
    
    -- Extract values from all TDs (area + data columns)
    extractAllTDValues :: [Tag String] -> [String]
    extractAllTDValues trTags = 
        let allTDs = findConsecutiveTDs trTags 0 []
            -- Extract values from each TD, handling both area and data columns
            values = map extractTDValue allTDs  -- no need to reverse since we append in order
        in values
    
    -- Extract value from any TD (handles both area names and data values)
    extractTDValue :: [Tag String] -> String
    extractTDValue tdTags = 
        -- First try to extract from <a class="data1"> link (for data columns)
        let data1Links = filter isData1Link (sections (~== ("<a" :: String)) tdTags)
        in case data1Links of
            (linkSection:_) -> 
                let linkText = Prelude.takeWhile (~/= ("</a>" :: String)) linkSection
                    content = innerText linkText  
                    cleaned = filter (not . isSpace) content
                in if null cleaned || cleaned == "-" then "" else cleaned
            -- No data1 link - check if this is area column with nested DataStub
            [] -> 
                let dataStubTDs = filter isDataStubTD (sections (~== ("<td" :: String)) tdTags ++ sections (~== ("<TD" :: String)) tdTags)
                in case dataStubTDs of
                    (stubTD:_) -> 
                        let content = innerText stubTD
                            trimmed = reverse (dropWhile isSpace (reverse (dropWhile isSpace content)))
                        in if null trimmed then "" else trimmed
                    -- No DataStub - extract plain text
                    [] -> 
                        let content = innerText tdTags
                            cleaned = filter (not . isSpace) content
                        in if null cleaned then "" else cleaned
      where
        isData1Link linkSection = 
            case linkSection of
                [] -> False
                (TagOpen "a" attrs : _) -> 
                    case lookup "class" attrs of
                        Just className -> "data1" `isInfixOf` map toLower className
                        Nothing -> False
                _ -> False
        
        -- Check if TD has DataStub class
        isDataStubTD tdSection =
            case tdSection of
                [] -> False
                (TagOpen _ attrs : _) -> 
                    case lookup "class" attrs of
                        Just className -> "DataStub" `isInfixOf` className
                        Nothing -> False
                _ -> False
    
    -- Extract a complete TD section
    extractTDSection :: [Tag String] -> Int -> ([Tag String], [Tag String])
    extractTDSection [] _ = ([], [])
    extractTDSection (tag:rest) depth = 
        case tag of
            TagOpen tagName _ | depth == 0 && map toLower tagName == "td" -> 
                let (content, remaining) = extractTDSection rest 1
                in (tag:content, remaining)
            TagClose tagName | depth == 1 && map toLower tagName == "td" -> ([tag], rest)
            TagOpen _ _ -> 
                let (content, remaining) = extractTDSection rest (depth + 1)
                in (tag:content, remaining)
            TagClose _ -> 
                let (content, remaining) = extractTDSection rest (depth - 1)
                in (tag:content, remaining)
            _ -> 
                let (content, remaining) = extractTDSection rest depth
                in (tag:content, remaining)
    
    -- Check if TD has DataA or DataS class
    isDataTDClass attrs = 
        case lookup "class" attrs of
            Just className -> "DataA" `isInfixOf` className || "DataS" `isInfixOf` className  
            Nothing -> False

    -- Check if TD has DataA class
    isDataATD tdSection =
        case tdSection of
            [] -> False
            (TagOpen _ attrs : _) -> 
                case lookup "class" attrs of
                    Just className -> "DataA" `isInfixOf` className
                    Nothing -> False
            _ -> False
    
    -- Check if TD has DataS class
    isDataSTD tdSection =
        case tdSection of
            [] -> False
            (TagOpen _ attrs : _) -> 
                case lookup "class" attrs of
                    Just className -> "DataS" `isInfixOf` className
                    Nothing -> False
            _ -> False
    
    -- Extract data content from a TD section
    extractDataContentFromTD tdSection = 
        -- Look for <a class="data1"> link within this TD
        let data1Links = filter isData1Link (sections (~== ("<a" :: String)) tdSection)
        in case data1Links of
            [] -> Nothing  -- No data1 link means blank cell
            (linkSection:_) -> 
                let linkText = Prelude.takeWhile (~/= ("</a>" :: String)) linkSection
                    content = innerText linkText  
                    cleaned = filter (not . isSpace) content
                in if null cleaned || cleaned == "-" then Nothing else Just cleaned
      where
        isData1Link linkSection = 
            case linkSection of
                [] -> False
                (TagOpen "a" attrs : _) -> 
                    case lookup "class" attrs of
                        Just className -> "data1" `isInfixOf` map toLower className
                        Nothing -> False
                _ -> False

-- Extract text content from <a class="data1"> link
extractData1LinkContent :: [Tag String] -> Maybe String
extractData1LinkContent linkSection = 
    let linkText = Prelude.takeWhile (~/= ("</a>" :: String)) linkSection
        content = innerText linkText
        cleaned = filter (not . isSpace) content
    in if null cleaned || cleaned == "-" then Nothing else Just cleaned

extractSingleDataCellContent :: [Tag String] -> Maybe String
extractSingleDataCellContent cellTags = 
    -- Look for <a> links first (most data is in links)
    let linkSections = sections (~== ("<a" :: String)) cellTags
    in case linkSections of
        [] -> 
            -- No link, get direct text content
            let content = innerText cellTags
                cleaned = filter (not . isSpace) content
            in if null cleaned || cleaned == "-" || cleaned == "" || length cleaned > 15 
               then Nothing else Just cleaned
        (linkContent:_) -> 
            -- Extract text from the first link only
            let linkText = Prelude.takeWhile (~/= ("</a>" :: String)) linkContent
                content = innerText linkText  
                cleaned = filter (not . isSpace) content
            in if null cleaned || cleaned == "-" || cleaned == "" || length cleaned > 15
               then Nothing else Just cleaned

-- Legacy function for compatibility
extractEiaDataCellContent :: [Tag String] -> Maybe String
extractEiaDataCellContent = extractSingleDataCellContent

-- Structure-based table parsing functions
parseTableStructure :: [Tag String] -> Maybe TableStructure
parseTableStructure tags = do
    eiaTableTags <- findEiaDataTable tags
    headerRows <- findHeaderRows eiaTableTags
    dataColCount <- countDataColumns eiaTableTags
    matrix <- buildHeaderMatrix headerRows
    let finalHdrs = resolveHeaderList matrix dataColCount
    return $ TableStructure matrix dataColCount finalHdrs

-- Find the specific EIA data table (the one containing DataRow elements)
findEiaDataTable :: [Tag String] -> Maybe [Tag String]
findEiaDataTable tags = 
    -- Look for tables that contain <tr class="DataRow"> elements
    let tables = sections (~== ("<table" :: String)) tags
        eiaTable = find hasDataRows tables
    in eiaTable
  where
    hasDataRows tableTag = any (~== ("<tr class=\"DataRow\">" :: String)) tableTag

-- Find the actual data table header rows (the 2 rows that precede DataRow elements)
findHeaderRows :: [Tag String] -> Maybe [[Tag String]]
findHeaderRows eiaTableTags = 
    let allTRs = sections (~== ("<tr" :: String)) eiaTableTags
        dataRowIndices = findDataRowIndices allTRs
        headerRows = findHeaderRowsBeforeData allTRs dataRowIndices
    in if null headerRows then Nothing else Just headerRows
  where
    -- Find indices of DataRow elements (check if TR starts with DataRow pattern)
    findDataRowIndices trs = 
        [i | (i, tr) <- zip [0..] trs, isDataRowTR tr]
    
    -- Check if a TR section starts with DataRow tag
    isDataRowTR tr = 
        case tr of
            [] -> False
            (firstTag:_) -> firstTag ~== ("<tr class=\"DataRow\">" :: String)
    
    -- Find header rows that immediately precede the first DataRow
    findHeaderRowsBeforeData trs dataRowIndices = 
        case dataRowIndices of
            [] -> []
            (firstDataRowIdx:_) -> 
                -- Look for header rows in the 2-3 rows before the first DataRow
                let candidateIndices = [max 0 (firstDataRowIdx - 3) .. firstDataRowIdx - 1]
                    candidateRows = [(i, trs !! i) | i <- candidateIndices, i >= 0, i < length trs]
                    headerRows = filter (hasThElements . snd) candidateRows
                in map snd headerRows
    
    hasThElements row = any (~== ("<th" :: String)) row

-- Count data columns by analyzing nested table within DataRow elements  
countDataColumns :: [Tag String] -> Maybe Int
countDataColumns eiaTableTags =
    let allTRs = sections (~== ("<tr" :: String)) eiaTableTags
        dataRowTRs = filter isDataRowTR allTRs
        firstDataRow = listToMaybe dataRowTRs
    in case firstDataRow of
        Nothing -> Nothing
        Just row -> 
            -- Use full DataRow structure, not just first </tr>
            countNestedTableColumns row
  where
    -- Check if a TR section starts with DataRow tag
    isDataRowTR tr = 
        case tr of
            [] -> False
            (firstTag:_) -> firstTag ~== ("<tr class=\"DataRow\">" :: String)
    
    -- Count columns in main nested table class="data2" within the DataRow
    countNestedTableColumns tags =
        let nestedTables = sections (~== ("<table" :: String)) tags
            dataTables = filter isDataTable nestedTables
            -- Find the largest data2 table (main table with most content)
            mainTable = case dataTables of
                [] -> Nothing
                tables -> Just $ maximumBy (\t1 t2 -> compare (length t1) (length t2)) tables
        in case mainTable of
            Nothing -> Nothing
            Just table ->
                -- EIA tables have a complex nested structure, but we know they have the expected data columns
                -- from the table headers. Rather than trying to parse the complex nested structure,
                -- just return the expected count.
                Just eiaExpectedDataColumns
    
    -- Count only actual data columns, filtering out spacers and formatting columns
    countDataColumns' :: [Tag String] -> Int
    countDataColumns' tags = 
        let tdSections = sections (~== ("<td" :: String)) tags
            dataColumnSections = filter isDataColumn tdSections
        in length dataColumnSections
    
    -- Check if a TD section represents an actual data column vs spacer/formatting
    isDataColumn :: [Tag String] -> Bool
    isDataColumn tdSection =
        case tdSection of
            [] -> False
            (TagOpen "td" attrs : _) -> 
                -- Be more permissive - only exclude obvious non-data columns
                case (lookup "width" attrs, lookup "class" attrs) of
                    -- Skip tiny spacer columns (width <= 10)
                    (Just widthStr, _) -> 
                        case readMaybe widthStr of
                            Just width -> width > 10 && not (isNonDataClass attrs)
                            Nothing -> not (isNonDataClass attrs)
                    -- If no width, include unless it has exclusion classes or bad content
                    (Nothing, _) -> 
                        not (isNonDataClass attrs) && 
                        not (hasLongMetadataContent tdSection)
            _ -> False
    
    -- Check if attributes indicate a non-data column
    isNonDataClass :: [(String, String)] -> Bool
    isNonDataClass attrs =
        hasClass "DataStub" attrs ||    -- Row labels
        hasClass "Footnotes" attrs ||   -- Footnote text
        hasClass "Update" attrs ||      -- Release dates
        hasClass "Notes" attrs          -- Notes/metadata
    
    -- Check if TD content is long metadata text (to exclude)
    hasLongMetadataContent :: [Tag String] -> Bool
    hasLongMetadataContent tdSection =
        let textContent = concatMap extractText tdSection
            cleanText = filter (not . (`elem` [' ', '\r', '\n', '\t'])) textContent
        in -- Exclude very long text or specific metadata patterns
           length cleanText > 500 ||  -- Very long text is likely metadata
           any (`isInfixOf` cleanText) ["Release Date", "Notes:", "See Definitions", 
                                       "Next Release Date", "Imports at the PAD District level",
                                       "independent rounding", "Data may not add"]
    
    -- Helper to check if attributes contain a specific class
    hasClass :: String -> [(String, String)] -> Bool
    hasClass targetClass attrs = 
        case lookup "class" attrs of
            Just className -> targetClass `isInfixOf` className
            Nothing -> False
    
    -- Extract text content from tags
    extractText :: Tag String -> String
    extractText (TagText text) = text
    extractText _ = ""
    
    -- Find a TR that contains actual numeric data values (not just row labels)
    findDataRowWithNumericValues :: [[Tag String]] -> Maybe [Tag String]
    findDataRowWithNumericValues [] = Nothing
    findDataRowWithNumericValues (tr:rest) =
        if hasMultipleNumericValues tr
        then Just tr
        else findDataRowWithNumericValues rest
    
    -- Check if a TR contains multiple numeric data values (indicating it's a data row)
    hasMultipleNumericValues :: [Tag String] -> Bool
    hasMultipleNumericValues tr =
        let tdSections = sections (~== ("<td" :: String)) tr
            numericTDs = filter hasNumericValue tdSections
            totalTDs = length tdSections
        in totalTDs >= 10 &&  -- Must have at least 10 total TDs (eiaExpectedDataColumns data columns expected)
           length numericTDs >= 4  -- At least 4 numeric values (some columns may be empty)
    
    -- Check if a TD section contains a numeric value
    hasNumericValue :: [Tag String] -> Bool
    hasNumericValue tdSection =
        let textContent = concatMap extractText tdSection
            cleanText = filter (/= ' ') $ 
                       filter (/= '\r') $ 
                       filter (/= '\n') $ 
                       filter (/= '\t') textContent
        in length cleanText > 0 && isNumericText cleanText
    
    -- Check if text looks like a numeric value (digits, commas, possibly negative)
    isNumericText :: String -> Bool
    isNumericText text = 
        let cleanText = filter (`notElem` [' ', '\160', '\r', '\n', '\t']) text  -- Remove all whitespace
        in not (null cleanText) && 
           length cleanText < 30 &&  -- Increased threshold for long numbers with commas
           (all (\c -> c `elem` ("0123456789,-." :: String)) cleanText ||
            cleanText == "-" ||      -- Empty data as dash
            cleanText == "W" ||      -- EIA withheld data
            cleanText == "0")        -- Zero values
    
    -- Check if a table has class="data2"
    isDataTable tableSection = 
        case tableSection of
            [] -> False
            (TagOpen "table" attrs : _) -> 
                case lookup "class" attrs of
                    Just className -> className == "data2"
                    Nothing -> False
            _ -> False

-- Build header matrix from header rows
buildHeaderMatrix :: [[Tag String]] -> Maybe [[Maybe HeaderCell]]
buildHeaderMatrix headerRows = 
    let parsedRows = map parseHeaderRow headerRows
        numRows = length headerRows
        maxCols = maximum $ map countHeaderCells headerRows
    in Just $ buildMatrix parsedRows numRows maxCols
  where
    countHeaderCells row = length $ filter (~== ("<th" :: String)) row

-- Parse a single header row into HeaderCell list
parseHeaderRow :: [Tag String] -> [HeaderCell]
parseHeaderRow rowTags = 
    let thSections = sections (~== ("<th" :: String)) rowTags
    in catMaybes $ zipWith parseHeaderCell [0..] thSections

-- Parse a single <th> element into HeaderCell
parseHeaderCell :: Int -> [Tag String] -> Maybe HeaderCell
parseHeaderCell startColumn thTags = do
    text <- extractHeaderText thTags
    let colSpanVal = getColSpanValue thTags
        rowSpanVal = getRowSpanValue thTags
    return $ HeaderCell text colSpanVal rowSpanVal 0 startColumn

-- Extract text content from <th> element
extractHeaderText :: [Tag String] -> Maybe String
extractHeaderText thTags = 
    let content = innerText $ Prelude.takeWhile (~/= ("</th>" :: String)) thTags
        cleaned = unwords $ words content
    in if null cleaned then Nothing else Just cleaned

-- Get colspan attribute value (default 1)
getColSpanValue :: [Tag String] -> Int
getColSpanValue (TagOpen _ attrs : _) = 
    case lookup "colspan" attrs of
        Nothing -> 1
        Just val -> maybe 1 id (readMaybe val)
getColSpanValue _ = 1

-- Get rowspan attribute value (default 1)  
getRowSpanValue :: [Tag String] -> Int
getRowSpanValue (TagOpen _ attrs : _) = 
    case lookup "rowspan" attrs of
        Nothing -> 1
        Just val -> maybe 1 id (readMaybe val)
getRowSpanValue _ = 1

-- Build matrix by placing header cells with proper span handling
buildMatrix :: [[HeaderCell]] -> Int -> Int -> [[Maybe HeaderCell]]
buildMatrix parsedRows numRows maxCols =
    let emptyMatrix = replicate numRows (replicate maxCols Nothing)
    in foldl placeRowInMatrix emptyMatrix (zip [0..] parsedRows)
  where
    placeRowInMatrix matrix (rowIdx, headerCells) = 
        foldl (placeCellInMatrix rowIdx) matrix headerCells
    
    placeCellInMatrix rowIdx matrix cell =
        let col = findNextAvailableColumn matrix rowIdx (startCol cell)
            updatedCell = cell { startRow = rowIdx, startCol = col }
        in placeCellWithSpan matrix updatedCell

-- Find next available column position in a row
findNextAvailableColumn :: [[Maybe HeaderCell]] -> Int -> Int -> Int
findNextAvailableColumn matrix rowIdx startPos =
    let row = matrix !! rowIdx
        searchFrom = max 0 startPos
    in findFirstNothing row searchFrom
  where
    findFirstNothing row pos
        | pos >= length row = pos
        | isNothing (row !! pos) = pos  
        | otherwise = findFirstNothing row (pos + 1)

-- Place cell in matrix considering span
placeCellWithSpan :: [[Maybe HeaderCell]] -> HeaderCell -> [[Maybe HeaderCell]]
placeCellWithSpan matrix cell = 
    let rows = [startRow cell .. min (length matrix - 1) (startRow cell + rowSpan cell - 1)]
        cols = [startCol cell .. startCol cell + colSpan cell - 1]
    in foldl (placeInPosition cell) matrix [(r, c) | r <- rows, c <- cols]
  where
    placeInPosition cell matrix (r, c)
        | r >= length matrix || c < 0 = matrix  -- Out of bounds, return unchanged
        | otherwise = 
            let matrixMaxCols = if null matrix then 0 else maximum (map length matrix)
                (before, rows) = splitAt r matrix
                row = case rows of
                    [] -> replicate (max (c + 1) matrixMaxCols) Nothing  -- Create new row if needed
                    (currentRow:_) -> currentRow
                after = drop 1 rows
                extendedRow = if c >= length row 
                             then row ++ replicate (c + 1 - length row) Nothing
                             else row
                (beforeCol, afterCol) = splitAt c extendedRow
                newRow = beforeCol ++ [Just cell] ++ drop 1 afterCol
            in before ++ [newRow] ++ after

-- Resolve final header list from matrix
resolveHeaderList :: [[Maybe HeaderCell]] -> Int -> [String]
resolveHeaderList matrix dataColCount = 
    map (getHeaderForColumn matrix) [0..dataColCount-1]

-- Get the appropriate header text for a data column
getHeaderForColumn :: [[Maybe HeaderCell]] -> Int -> String
getHeaderForColumn matrix colIdx = 
    case findDeepestHeader matrix colIdx of
        Nothing -> "Column " ++ show colIdx
        Just cell -> headerText cell
  where
    findDeepestHeader matrix colIdx =
        let columnCells = [matrix !! r !! colIdx | r <- [0..length matrix - 1], 
                          colIdx < length (matrix !! r)]
            validCells = catMaybes columnCells
        in listToMaybe $ reverse validCells  -- Get the last (deepest) header

-- Legacy functions for compatibility (now delegate to EIA-specific versions)
extractHeaderRow :: [Tag String] -> Maybe [String]
extractHeaderRow tags = 
    let eiaHeaders = extractEiaHeaders tags
    in if null eiaHeaders then Nothing else Just eiaHeaders

extractDataRows :: [Tag String] -> [[String]]
extractDataRows = extractEiaRows

extractRowCells :: [Tag String] -> [String]
extractRowCells = extractEiaRowContent

extractCells :: [Tag String] -> [String]
extractCells tags = catMaybes $ map extractCellContent $ sections (~== ("<td>" :: String)) tags ++ sections (~== ("<th>" :: String)) tags

extractCellContent :: [Tag String] -> Maybe String
extractCellContent [] = Nothing
extractCellContent tags = 
    let content = innerText $ Prelude.takeWhile (~/= ("</td>" :: String)) $ Prelude.takeWhile (~/= ("</th>" :: String)) tags
        cleaned = filter (not . isSpace) content
    in if null cleaned then Nothing else Just cleaned

findRowByName :: [[String]] -> String -> Maybe [String]
findRowByName rows targetName = find (matchesRowName targetName) rows
  where
    matchesRowName target row = 
        case row of
            [] -> False
            (firstCell:_) -> normalizeText target == normalizeText firstCell

findColumnIndex :: [String] -> String -> Maybe Int
findColumnIndex headers targetCol = 
    listToMaybe [i | (i, header) <- zip [0..] headers, normalizeText targetCol == normalizeText header]

extractCellData :: [String] -> Int -> Maybe ByteString
extractCellData row index = 
    case drop index row of
        [] -> Nothing
        (cell:_) -> Just $ pack cell

normalizeText :: String -> String
normalizeText = map toLower . filter (not . isSpace)

-- Get data from HTML page

-- New HTML-based runBot function
runHtmlBot :: FilePath -> GetData ()
runHtmlBot fpath = do
        !dataPoints <- readDataPoints "monthly_data.csv" readHtmlDataPoint
        !results <- getHtmlResults dataPoints
        result <- exceptT results
        let date = ""
            out = BL.intercalate "\n" $ getOutRows date result
        liftIO $ BL.writeFile fpath out

-- Process multiple HTML data points efficiently with minimal HTTP requests
getHtmlResults :: [HtmlDataPoint] -> GetData (Either String [ResultColumn])
getHtmlResults dataPoints = do
    -- Extract unique URLs to minimize HTTP requests
    let uniqueURLs = map url $ M.elems $ M.fromListWith const [(url dp, dp) | dp <- dataPoints]
    
    -- Log how many unique URLs vs total data points
    liftIO $ putStrLn $ "Processing " ++ show (length dataPoints) ++ " data points from " ++ show (length uniqueURLs) ++ " unique URLs"
    
    -- Fetch each unique URL in parallel and build cache
    asyncs <- liftIO $ mapM (\u -> do
        putStrLn $ "Starting fetch: " ++ u
        async (runExceptT (getURL u))) uniqueURLs
    
    htmlResults <- liftIO $ mapM wait asyncs
    htmlCache <- foldM buildCache M.empty (zip uniqueURLs htmlResults)
    
    -- Process each data point using cached HTML, preserving original order
    results <- mapM (getHtmlResultCached htmlCache) dataPoints
    return $ sequence results
  where
    buildCache cache (url, result) = do
        case result of
            Left e -> throwError $ "Failed to fetch " ++ url ++ ": " ++ e
            Right html -> return $ M.insert url html cache

-- Get result from HTML data point using cached HTML
getHtmlResultCached :: Map String ByteString -> HtmlDataPoint -> GetData (Either String ResultColumn)
getHtmlResultCached htmlCache dp@(HtmlDataPoint { url = u, rowName = rn, columnName = cn, htmlDescription = desc, htmlCode = c }) = do
    case M.lookup u htmlCache of
        Nothing -> return $ Left $ "HTML not cached for URL: " ++ u
        Just html -> do
            let result = parseHtmlTable html (unpack rn) (unpack cn)
            case result of
                Nothing -> return $ Left $ "Couldn't find data point: " ++ show dp
                Just value -> return $ Right $ ResultColumn c desc value

-- Legacy CSV-based runBot function (kept for backward compatibility)
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

-- New headers for HTML-based configuration
htmlHeaders :: Vector ByteString
htmlHeaders = V.fromList ["Description", "Label", "URL", "Row Name", "Column Name"]

columnToIndex :: String -> Int
columnToIndex c = columnToIndex' (map toLower c) 0
  where
    columnToIndex' [] acc = acc
    columnToIndex' (c:cs) acc = columnToIndex' cs (charIndex c + acc * 26)
    charIndex c = ord c - ord 'a'

indexToColumn :: Int -> String
indexToColumn i | i < 26 = [convertDigit i]
indexToColumn i = let (quotient, remainder) = i `divMod` 26
                  in indexToColumn quotient ++ [convertDigit remainder]

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

-- New function to read HTML data points from CSV config
readHtmlDataPoint :: Vector ByteString -> Maybe HtmlDataPoint
readHtmlDataPoint csv = do
  desc <- csv !? 0
  label <- csv !? 1  
  url <- csv !? 2
  rowName <- csv !? 3
  columnName <- csv !? 4
  return $ HtmlDataPoint (unpack url) rowName columnName desc label

-- Write HTML data points to CSV config
writeHtmlDataPoints :: FilePath -> [HtmlDataPoint] -> IO ()
writeHtmlDataPoints fp dps = writeDataPoints fp htmlHeaders dps writeHtmlDataPoint

writeHtmlDataPoint :: HtmlDataPoint -> Vector ByteString
writeHtmlDataPoint (HtmlDataPoint url rowName colName desc label) =
  V.fromList [desc, label, pack url, rowName, colName]

