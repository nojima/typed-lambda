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
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec.Pos (SourcePos, sourcePosPretty)

data Term
    = Bool     SourcePos Bool
    | Int      SourcePos Integer
    | If       SourcePos Term Term Term
    | Variable SourcePos Identifier
    | Lambda   SourcePos Identifier Term
    | Apply    SourcePos Term Term
    | BinOp    SourcePos Operator Term Term
    | Let      SourcePos Identifier Term Term
    | Def      SourcePos Identifier Identifier Term Term
    | List     SourcePos [Term]
    deriving (Show, Eq)

data Operator
    = Add -- '+'
    | Sub -- '-'
    | Mul -- '*'
    | Div -- '/'
    | And -- '&&'
    | Or  -- '||'
    | Equal -- '=='
    deriving (Show, Eq)

sourcePos :: Term -> SourcePos
sourcePos term =
    case term of
        Bool sp _ -> sp
        Int sp _ -> sp
        If sp _ _ _ -> sp
        Variable sp _ -> sp
        Lambda sp _ _ -> sp
        Apply sp _ _ -> sp
        BinOp sp _ _ _ -> sp
        Let sp _ _ _ -> sp
        Def sp _ _ _ _ -> sp
        List sp _ -> sp

mapSourcePos :: (SourcePos -> SourcePos) -> Term -> Term
mapSourcePos f term =
    case term of
        Bool sp bool -> Bool (f sp) bool
        Int sp nat -> Int (f sp) nat
        If sp c t e -> If (f sp) (mapSourcePos f c) (mapSourcePos f t) (mapSourcePos f e)
        Variable sp i -> Variable (f sp) i
        Lambda sp a b -> Lambda (f sp) a (mapSourcePos f b)
        Apply sp fn a -> Apply (f sp) (mapSourcePos f fn) (mapSourcePos f a)
        BinOp sp op t1 t2 -> BinOp (f sp) op (mapSourcePos f t1) (mapSourcePos f t2)
        Let sp vn ve b -> Let (f sp) vn (mapSourcePos f ve) (mapSourcePos f b)
        Def sp name arg expr b -> Def (f sp) name arg (mapSourcePos f expr) (mapSourcePos f b)
        List sp terms -> List (f sp) (map (mapSourcePos f) terms)

pretty :: Int -> Term -> Text
pretty indentLevel term =
    case term of
        Bool _ bool ->
            T.pack (show bool)

        Int _ nat ->
            T.pack (show nat)

        If _ condTerm thenTerm elseTerm ->
            "(IF "
            <> pretty (indentLevel + 1) condTerm
            <> "\n"
            <> indent indentLevel
            <> pretty (indentLevel + 1) thenTerm
            <> "\n"
            <> indent indentLevel
            <> pretty (indentLevel + 1) elseTerm
            <> ")"

        Variable _ identifier ->
            Identifier.name identifier

        Lambda _ argumentName body ->
            "(LAMBDA "
            <> Identifier.name argumentName
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

        Let _ name expr body ->
            "LET "
            <> Identifier.name name
            <> " =\n"
            <> indent indentLevel
            <> pretty (indentLevel + 1) expr
            <> "\n"
            <> indent indentLevel
            <> "IN\n"
            <> indent indentLevel
            <> pretty (indentLevel + 1) body

        Def _ name arg expr body ->
            "DEF "
            <> Identifier.name name
            <> " "
            <> Identifier.name arg
            <> "\n"
            <> indent indentLevel
            <> pretty (indentLevel + 1) expr
            <> "\n"
            <> indent indentLevel
            <> "IN\n"
            <> indent indentLevel
            <> pretty (indentLevel + 1) body

        List _ elements ->
            "LIST ["
            <> T.intercalate "," (map (pretty (indentLevel + 1)) elements)
            <> "]"

operatorPretty :: Operator -> Text
operatorPretty operator =
    case operator of
        Add -> "+"
        Sub -> "-"
        Mul -> "*"
        Div -> "/"
        And -> "&&"
        Or  -> "||"
        Equal -> "=="

indent :: Int -> Text
indent level =
    T.replicate level "|   "
