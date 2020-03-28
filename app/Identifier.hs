module Identifier (Identifier(..)) where

import Data.Text (Text)

newtype Identifier = Identifier Text
    deriving (Show, Eq)
