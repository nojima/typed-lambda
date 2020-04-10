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
import qualified Data.Text as T

data Type
    = Bool
    | Int
    | List Type
    | Tuple [Type]
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

        Tuple elements ->
            "Tuple[" <> T.intercalate "," (map pretty elements) <> "]"

        Function arg ret ->
            "(" <> pretty arg <> " -> " <> pretty ret <> ")"

        Var identifier ->
            Identifier.name identifier
