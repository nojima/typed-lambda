{-# LANGUAGE OverloadedStrings #-}
module Type
    ( Type(..)
    , Variable
    , TypeScheme(..)
    , mapVariable
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
    deriving (Show, Eq)

mapVariable :: (Variable -> Type) -> Type -> Type
mapVariable f type_ =
    case type_ of
        Bool ->
            Bool

        Int ->
            Int

        List element ->
            List (mapVariable f element)

        Tuple elements ->
            Tuple (map (mapVariable f) elements)

        Function arg ret ->
            Function (mapVariable f arg) (mapVariable f ret)

        Var var ->
            f var

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
