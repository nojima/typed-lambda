{-# LANGUAGE OverloadedStrings #-}
module Eval (eval, run, RuntimeError(..)) where

import           Term (Term)
import qualified Term
import           Identifier (Identifier(..))
import           Value (Value, Frame(..))
import qualified Value
import           Data.Text (Text)
import qualified Data.Text as T

newtype RuntimeError = RuntimeError Text

lookupVariable :: Identifier -> Frame -> Maybe Value
lookupVariable identifier (Frame argumentName argumentValue parent) =
    if identifier == argumentName
        then Just argumentValue
        else lookupVariable identifier =<< parent

evalVariable :: Frame -> Identifier -> Either RuntimeError Value
evalVariable frame identifier =
    case lookupVariable identifier frame of
        Just term ->
            Right term

        Nothing ->
            let
                errorMessage =
                    "undefined variable '"
                    <> Identifier.name identifier
                    <> "'"
            in
            Left (RuntimeError errorMessage)

evalApply :: Frame -> Term -> Term -> Either RuntimeError Value
evalApply frame function argument = do
    functionValue <- eval frame function
    argumentValue <- eval frame argument

    case functionValue of
        Value.Closure closureFrame argumentName body ->
            let
                newFrame = Frame argumentName argumentValue (Just closureFrame)
            in
            eval newFrame body

        value ->
            let
                errorMessage =
                    "cannot apply non-function '"
                    <> T.pack (show value)
                    <> "'"
            in
            Left $ RuntimeError errorMessage

eval :: Frame -> Term -> Either RuntimeError Value
eval frame term =
    case term of
        Term.Bool value ->
            Right $ Value.Bool value

        Term.Variable identifier ->
            evalVariable frame identifier

        Term.Lambda identifier _ body ->
            Right $ Value.Closure frame identifier body

        Term.Apply function argument ->
            evalApply frame function argument

run :: Term -> Either RuntimeError Value
run term =
    let
        topFrame = Frame (Identifier "") (Value.Bool False) Nothing
    in
    eval topFrame term
