{-# LANGUAGE OverloadedStrings #-}
module Type
    ( Type(..)
    , pretty
    ) where

import           Identifier (Identifier)
import qualified Identifier
import           Data.Text (Text)

data Type
    = Bool
    | Int
    | Function Type Type
    | Variable Identifier
    deriving (Show, Eq)

pretty :: Type -> Text
pretty type_ =
    case type_ of
        Bool ->
            "Bool"

        Int ->
            "Int"

        Function arg ret ->
            pretty arg <> " -> " <> pretty ret

        Variable identifier ->
            Identifier.name identifier
