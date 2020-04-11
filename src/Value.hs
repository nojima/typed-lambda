module Value (Value(..), Frame, RuntimeError(..)) where

import Term (Term)
import qualified Term
import Identifier (Identifier)
import qualified Identifier
import qualified Data.Text as T
import qualified Data.Vector as Vector
import qualified Data.Map.Strict as Map

data Value
    = Bool Bool
    | Int Integer
    | List [Value]
    | Tuple (Vector.Vector Value)
    | Closure Frame Identifier Term
    | NativeFunction Identifier (Value -> Either RuntimeError Value)

newtype RuntimeError = RuntimeError T.Text

instance Show Value where
    show value =
        case value of
            Closure _ name body ->
                "Closure "
                ++ T.unpack (Identifier.name name)
                ++ " "
                ++ T.unpack (Term.pretty 1 body)

            NativeFunction name _ ->
                "NativeFunction " ++ T.unpack (Identifier.name name)

            List elements ->
                "List " ++ show elements

            Tuple elements ->
                "Tuple " ++ show elements

            Bool bool ->
                "Bool " ++ show bool

            Int int ->
                "Int " ++ show int

type Frame = Map.Map Identifier Value
