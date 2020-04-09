module Value (Value(..), Frame(..)) where

import Term (Term)
import qualified Term
import Identifier (Identifier)
import qualified Identifier
import qualified Data.Text as T

data Value
    = Bool Bool
    | Int Integer
    | Closure Frame Identifier Term
    deriving (Eq)

instance Show Value where
    show value =
        case value of
            Closure _ name body ->
                "Closure _ "
                ++ T.unpack (Identifier.name name)
                ++ " "
                ++ T.unpack (Term.pretty 1 body)
            Bool bool ->
                "Bool " ++ show bool
            Int int ->
                "Int " ++ show int

data Frame = Frame
    { variableName  :: Identifier
    , variableValue :: Value
    , parentFrame   :: Maybe Frame
    }
    deriving (Show, Eq)
