{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Term
import           Parse (parse)
import qualified Eval
import qualified System.IO as IO  
import qualified System.Exit as Exit
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    source <- TIO.getContents
    TIO.hPutStr IO.stderr $ "Source:\n----------\n" <> source <> "\n----------\n"

    ast <- case parse source of
        Left errorMessage -> do
            IO.hPutStrLn IO.stderr "ERROR: failed to parse the given source code."
            IO.hPutStrLn IO.stderr errorMessage
            Exit.exitFailure
        Right result ->
            return result

    TIO.putStrLn $ "AST:\n" <> Term.pretty 1 ast

    value <- case Eval.run ast of
        Left (Eval.RuntimeError errorMessage) -> do
            TIO.hPutStrLn IO.stderr $ "ERROR: " <> errorMessage
            Exit.exitFailure
        Right result ->
            return result

    putStrLn $ "=> " ++ show value
