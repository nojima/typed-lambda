{-# LANGUAGE OverloadedStrings #-}
module Type (Type(..), pretty) where

import           Data.Text (Text)

data Type
    = Bool
    | Nat
    | Function Type Type
    deriving (Show, Eq)

pretty :: Type -> Text
pretty type_ =
    case type_ of
        Bool ->
            "Bool"

        Nat ->
            "Nat"

        Function arg ret ->
            pretty arg <> " -> " <> pretty ret
