module Identifier (Identifier(..), fromText) where

import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T

newtype Identifier = Identifier
    { name :: Text }
    deriving (Show, Eq, Ord)

fromText :: Text -> Identifier
fromText = Identifier

instance IsString Identifier where
    fromString str = Identifier (T.pack str)
