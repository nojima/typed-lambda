module Identifier (Identifier(..), fromText) where

import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T

newtype Identifier = Identifier
    { name :: Text }
    deriving (Eq, Ord)

fromText :: Text -> Identifier
fromText = Identifier

instance Show Identifier where
    show (Identifier n) = show (T.unpack n)

instance IsString Identifier where
    fromString str = Identifier (T.pack str)
