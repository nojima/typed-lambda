module Identifier (Identifier(..)) where

import Data.Text (Text)

newtype Identifier = Identifier
    { name :: Text }
    deriving (Show, Eq)
