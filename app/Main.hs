{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Term
import           Parse (parse)
import qualified Type
import           TypeCheck (typeCheck)
import qualified TypeCheck
import qualified Eval
import qualified System.IO as IO
import qualified System.Exit as Exit
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    source <- TIO.getContents
    TIO.hPutStr IO.stderr $ "----------\nSource:\n" <> source <> "\n"

    TIO.hPutStrLn IO.stderr "----------"
    ast <- case parse source of
        Left errorMessage -> do
            IO.hPutStrLn IO.stderr "SyntaxError: failed to parse the given source code."
            IO.hPutStrLn IO.stderr errorMessage
            Exit.exitFailure
        Right result ->
            return result

    TIO.putStrLn $ "AST:\n" <> Term.pretty 1 ast

    TIO.hPutStrLn IO.stderr "----------"
    case typeCheck ast of
        Left (TypeCheck.TypeError errorMessage) -> do
            TIO.hPutStrLn IO.stderr $ "TypeError: " <> errorMessage
            Exit.exitFailure
        Right result ->
            TIO.putStrLn $ "TypeCheck: OK\nType: " <> Type.pretty result

    TIO.hPutStrLn IO.stderr "----------"
    value <- case Eval.run ast of
        Left (Eval.RuntimeError errorMessage) -> do
            TIO.hPutStrLn IO.stderr $ "RuntimeError: " <> errorMessage
            Exit.exitFailure
        Right result ->
            return result

    putStrLn $ "=> " <> show value
