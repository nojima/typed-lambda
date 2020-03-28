{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Parse(parse)
import qualified System.IO as IO  
import qualified System.Exit as Exit
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    source <- TIO.getContents
    TIO.hPutStrLn IO.stderr $ "Source: " <> source

    case parse source of
        Left errorMessage -> do
            putStrLn "ERROR: failed to parse the given source code."
            putStrLn errorMessage
            Exit.exitFailure
        Right result ->
            print result
