{-# LANGUAGE OverloadedStrings #-}
module Type
    ( Type(..)
    , pretty
    ) where

import           Data.Text (Text)
import qualified Data.Text as T

data Type
    = Bool
    | Int
    | Function Type Type
    | Variable Int
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

        Variable id_ ->
            "t" <> T.pack (show id_)
