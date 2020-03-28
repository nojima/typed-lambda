{-# LANGUAGE OverloadedStrings #-}
module Type (Type(..), pretty) where

import           Data.Text (Text)

data Type
    = Bool
    | Function Type Type
    deriving (Show, Eq)

pretty :: Type -> Text
pretty type_ =
    case type_ of
        Bool ->
            "Bool"
        Function arg ret ->
            pretty arg <> " -> " <> pretty ret
