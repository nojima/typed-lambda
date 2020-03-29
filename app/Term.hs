{-# LANGUAGE OverloadedStrings #-}
module Term (Term(..), pretty) where

import           Identifier (Identifier)
import qualified Identifier
import           Type (Type)
import qualified Type
import           Data.Text (Text)
import qualified Data.Text as T

data Term
    = Bool Bool
    | Nat Integer
    | If Term Term Term
    | Variable Identifier
    | Lambda Identifier Type Term
    | Apply Term Term
    deriving (Show, Eq)

pretty :: Int -> Term -> Text
pretty indentLevel term =
    case term of
        Bool bool ->
            T.pack (show bool)

        Nat nat ->
            T.pack (show nat)

        If condTerm thenTerm elseTerm ->
            "(IF ("
            <> pretty (indentLevel + 1) condTerm
            <> ")\n"
            <> indent indentLevel
            <> pretty (indentLevel + 1) thenTerm
            <> "\n"
            <> indent indentLevel
            <> pretty (indentLevel + 1) elseTerm
            <> ")"

        Variable identifier ->
            Identifier.name identifier

        Lambda argumentName argumentType body ->
            "(LAMBDA "
            <> Identifier.name argumentName
            <> " : "
            <> Type.pretty argumentType
            <> " .\n"
            <> indent indentLevel
            <> pretty (indentLevel + 1) body
            <> ")"

        Apply function argument ->
            "(APPLY \n"
            <> indent indentLevel
            <> pretty (indentLevel + 1) function
            <> "\n"
            <> indent indentLevel
            <> pretty (indentLevel + 1) argument
            <> ")"

indent :: Int -> Text
indent level =
    T.replicate level "|   "
