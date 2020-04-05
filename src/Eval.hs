{-# LANGUAGE OverloadedStrings #-}
module Eval (eval, run, RuntimeError(..)) where

import           Term (Term, Operator)
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

evalIf :: Frame -> Term -> Term -> Term -> Either RuntimeError Value
evalIf frame condTerm thenTerm elseTerm = do
    condValue <- eval frame condTerm
    cond <- case condValue of
        Value.Bool bool ->
            return bool

        value ->
            let
                errorMessage =
                    "non-bool '"
                    <> T.pack (show value)
                    <> "' used as if condition"
            in
            Left $ RuntimeError errorMessage

    if cond then
        eval frame thenTerm
    else
        eval frame elseTerm

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

toInt :: Value -> Either RuntimeError Integer
toInt value =
    case value of
        Value.Int n ->
            Right n
        other ->
            let
                errorMessage =
                    "an integer value is expected, but "
                    <> T.pack (show other)
                    <> " is passed"
            in
            Left $ RuntimeError errorMessage

toBool :: Value -> Either RuntimeError Bool
toBool value =
    case value of
        Value.Bool b ->
            Right b
        other ->
            let
                errorMessage =
                    "a boolean value is expected, but "
                    <> T.pack (show other)
                    <>" is passed"
            in
            Left $ RuntimeError errorMessage

evalBinOp :: Frame -> Operator -> Term -> Term -> Either RuntimeError Value
evalBinOp frame operator lhs rhs = do
    lhsValue <- eval frame lhs
    rhsValue <- eval frame rhs

    case operator of
        Term.Add -> Value.Int  <$> ((+)  <$> toInt  lhsValue <*> toInt  rhsValue)
        Term.Sub -> Value.Int  <$> ((-)  <$> toInt  lhsValue <*> toInt  rhsValue)
        Term.Mul -> Value.Int  <$> ((*)  <$> toInt  lhsValue <*> toInt  rhsValue)
        Term.Div -> Value.Int  <$> (div  <$> toInt  lhsValue <*> toInt  rhsValue)
        Term.And -> Value.Bool <$> ((&&) <$> toBool lhsValue <*> toBool rhsValue)
        Term.Or  -> Value.Bool <$> ((||) <$> toBool lhsValue <*> toBool rhsValue)

eval :: Frame -> Term -> Either RuntimeError Value
eval frame term =
    case term of
        Term.Bool _ value ->
            Right $ Value.Bool value

        Term.Int _ value ->
            Right $ Value.Int value

        Term.If _ condTerm thenTerm elseTerm ->
            evalIf frame condTerm thenTerm elseTerm

        Term.Variable _ identifier ->
            evalVariable frame identifier

        Term.Lambda _ identifier _ body ->
            Right $ Value.Closure frame identifier body

        Term.Apply _ function argument ->
            evalApply frame function argument

        Term.BinOp _ operator lhs rhs ->
            evalBinOp frame operator lhs rhs

run :: Term -> Either RuntimeError Value
run term =
    let
        topFrame = Frame (Identifier "") (Value.Bool False) Nothing
    in
    eval topFrame term
