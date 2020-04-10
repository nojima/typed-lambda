{-# LANGUAGE OverloadedStrings #-}
module Type
    ( Type(..)
    , Variable
    , TypeScheme(..)
    , pretty
    ) where

import           Identifier (Identifier)
import qualified Identifier
import           Data.Text (Text)

data Type
    = Bool
    | Int
    | List Type
    | Function Type Type
    | Var Variable
    deriving (Show, Eq, Ord)

type Variable = Identifier

data TypeScheme = ForAll [Variable] Type
    deriving (Show)

pretty :: Type -> Text
pretty type_ =
    case type_ of
        Bool ->
            "Bool"

        Int ->
            "Int"

        List element ->
            "List[" <> pretty element <> "]"

        Function arg ret ->
            "(" <> pretty arg <> " -> " <> pretty ret <> ")"

        Var identifier ->
            Identifier.name identifier
