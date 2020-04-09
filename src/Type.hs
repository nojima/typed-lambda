{-# LANGUAGE OverloadedStrings #-}
module Type
    ( Type(..)
    , Variable
    , pretty
    ) where

import           Identifier (Identifier)
import qualified Identifier
import           Data.Text (Text)

data Type
    = Bool
    | Int
    | Function Type Type
    | Var Variable
    deriving (Show, Eq, Ord)

type Variable = Identifier

pretty :: Type -> Text
pretty type_ =
    case type_ of
        Bool ->
            "Bool"

        Int ->
            "Int"

        Function arg ret ->
            "(" <> pretty arg <> " -> " <> pretty ret <> ")"

        Var identifier ->
            Identifier.name identifier
