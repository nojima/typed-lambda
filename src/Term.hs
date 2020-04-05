{-# LANGUAGE OverloadedStrings #-}
module Term
    ( Term(..)
    , Operator(..)
    , pretty
    , sourcePos
    , mapSourcePos
    , sourcePosPretty
    , SourcePos
    ) where

import           Identifier (Identifier)
import qualified Identifier
import           Type (Type)
import qualified Type
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec.Pos (SourcePos, sourcePosPretty)

data Term
    = Bool     SourcePos Bool
    | Int      SourcePos Integer
    | If       SourcePos Term Term Term
    | Variable SourcePos Identifier
    | Lambda   SourcePos Identifier Type Term
    | Apply    SourcePos Term Term
    | BinOp    SourcePos Operator Term Term
    deriving (Show, Eq)

data Operator
    = Add -- '+'
    | Sub -- '-'
    | Mul -- '*'
    | Div -- '/'
    | And -- '&&'
    | Or  -- '||'
    deriving (Show, Eq)

sourcePos :: Term -> SourcePos
sourcePos term =
    case term of
        Bool sp _ -> sp
        Int sp _ -> sp
        If sp _ _ _ -> sp
        Variable sp _ -> sp
        Lambda sp _ _ _ -> sp
        Apply sp _ _ -> sp
        BinOp sp _ _ _ -> sp

mapSourcePos :: (SourcePos -> SourcePos) -> Term -> Term
mapSourcePos f term =
    case term of
        Bool sp bool -> Bool (f sp) bool
        Int sp nat -> Int (f sp) nat
        If sp c t e -> If (f sp) (mapSourcePos f c) (mapSourcePos f t) (mapSourcePos f e)
        Variable sp i -> Variable (f sp) i
        Lambda sp a t b -> Lambda (f sp) a t (mapSourcePos f b)
        Apply sp fn a -> Apply (f sp) (mapSourcePos f fn) (mapSourcePos f a)
        BinOp sp op t1 t2 -> BinOp (f sp) op (mapSourcePos f t1) (mapSourcePos f t2)

pretty :: Int -> Term -> Text
pretty indentLevel term =
    case term of
        Bool _ bool ->
            T.pack (show bool)

        Int _ nat ->
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

        BinOp _ operator lhs rhs ->
            "("
            <> pretty indentLevel lhs
            <> " "
            <> operatorPretty operator
            <> " "
            <> pretty indentLevel rhs
            <> ")"

operatorPretty :: Operator -> Text
operatorPretty operator =
    case operator of
        Add -> "+"
        Sub -> "-"
        Mul -> "*"
        Div -> "/"
        And -> "&&"
        Or  -> "||"

indent :: Int -> Text
indent level =
    T.replicate level "|   "
