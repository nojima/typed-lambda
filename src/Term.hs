{-# LANGUAGE OverloadedStrings #-}
module Term (Term(..), pretty, sourcePos, mapSourcePos, sourcePosPretty, SourcePos) where

import           Identifier (Identifier)
import qualified Identifier
import           Type (Type)
import qualified Type
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec.Pos (SourcePos, sourcePosPretty)

data Term
    = Bool     SourcePos Bool
    | Nat      SourcePos Integer
    | If       SourcePos Term Term Term
    | Variable SourcePos Identifier
    | Lambda   SourcePos Identifier Type Term
    | Apply    SourcePos Term Term
    deriving (Show, Eq)

sourcePos :: Term -> SourcePos
sourcePos term =
    case term of
        Bool sp _ -> sp
        Nat sp _ -> sp
        If sp _ _ _ -> sp
        Variable sp _ -> sp
        Lambda sp _ _ _ -> sp
        Apply sp _ _ -> sp

mapSourcePos :: (SourcePos -> SourcePos) -> Term -> Term
mapSourcePos f term =
    case term of
        Bool sp bool -> Bool (f sp) bool
        Nat sp nat -> Nat (f sp) nat
        If sp c t e -> If (f sp) (mapSourcePos f c) (mapSourcePos f t) (mapSourcePos f e)
        Variable sp i -> Variable (f sp) i
        Lambda sp a t b -> Lambda (f sp) a t (mapSourcePos f b)
        Apply sp fn a -> Apply (f sp) (mapSourcePos f fn) (mapSourcePos f a)

pretty :: Int -> Term -> Text
pretty indentLevel term =
    case term of
        Bool _ bool ->
            T.pack (show bool)

        Nat _ nat ->
            T.pack (show nat)

        If _ condTerm thenTerm elseTerm ->
            "(IF ("
            <> pretty (indentLevel + 1) condTerm
            <> ")\n"
            <> indent indentLevel
            <> pretty (indentLevel + 1) thenTerm
            <> "\n"
            <> indent indentLevel
            <> pretty (indentLevel + 1) elseTerm
            <> ")"

        Variable _ identifier ->
            Identifier.name identifier

        Lambda _ argumentName argumentType body ->
            "(LAMBDA "
            <> Identifier.name argumentName
            <> " : "
            <> Type.pretty argumentType
            <> " .\n"
            <> indent indentLevel
            <> pretty (indentLevel + 1) body
            <> ")"

        Apply _ function argument ->
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
