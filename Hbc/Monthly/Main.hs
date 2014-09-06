module Main (main) where

import Hbc.Monthly

import Control.Monad.Except

main :: IO ()
main = do
        result <- runExceptT $ runBot "0 Monthly Bot Output.csv"
        case result of
            Left e  -> putStrLn $ "Error: " ++ e
            _       -> putStrLn "Success!"


