module Main where

import Network.Wreq
import Control.Lens
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Char (ord)
import Data.List (intercalate)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T

type CsvData = V.Vector (V.Vector BL.ByteString)

fetchUrl :: String -> IO BL.ByteString
fetchUrl url = do
        response <- get url
        let body = response ^. responseBody
        return body

decodeCommaSeparated :: BL.ByteString -> Either String CsvData
decodeCommaSeparated = decodeWith opts NoHeader
  where
    opts :: DecodeOptions
    opts = defaultDecodeOptions { decDelimiter = fromIntegral (ord ',') }


str :: BL.ByteString
str = "\"Refiner and Blender Net Production \",\"Adjustment\",\"-1,026\",\"-731\",\"-70\",\"130\",\"-889\",\"-73\"\r\n\"Refiner and Blender Net Production \",\"Reformulated\",\"3,172\",\"3,162\",\"3,111\",\"3,117\",\"3,134\",\"3,063\"\r\n\"Refiner and Blender Net Production \",\"East Coast (PADD 1)\",\"1,271\",\"1,275\",\"1,262\",\"1,215\",\"1,259\",\"1,251\""
{-
\"Refiner and Blender Net Production \",\"Adjustment\",\"-1,026\",\"-731\",\"-70\",\"130\",\"-889\",\"-73\\r\n
-}

main :: IO ()
main = do
        t <- fetchUrl url
        putStr "from web: "
        check $ decodeCommaSeparated t  -- fails
        BL.writeFile "data.csv" t
        t' <- BL.readFile "data.csv"
        putStr "from file: "
        check $ decodeCommaSeparated t' -- succeeds
  where
    url = "http://ir.eia.gov/wpsr/table9.csv"
    check :: Either String CsvData -> IO ()
    check (Left e) = putStrLn $ "failed: " ++ e
    check _        = putStrLn "succeeded"

testData :: IO BL.ByteString
testData = do
        file <- readFile "data.csv.bak" :: IO String
        return . T.encodeUtf8 . T.intercalate "\r\n" . T.lines . T.pack $ file


