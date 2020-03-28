{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Parse(parse)
import qualified System.IO as IO  
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    source <- TIO.getContents
    TIO.hPutStrLn IO.stderr $ "Source: " <> source
    print $ parse source
