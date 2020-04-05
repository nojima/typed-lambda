module Value (Value(..), Frame(..)) where

import Term (Term)
import Identifier (Identifier)

data Value
    = Bool Bool
    | Int Integer
    | Closure Frame Identifier Term
    deriving (Show, Eq)

data Frame = Frame
    { variableName  :: Identifier
    , variableValue :: Value
    , parentFrame   :: Maybe Frame
    }
    deriving (Show, Eq)
