module Identifier (Identifier(..)) where

import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T

newtype Identifier = Identifier
    { name :: Text }
    deriving (Show, Eq, Ord)

instance IsString Identifier where
    fromString str = Identifier (T.pack str)
