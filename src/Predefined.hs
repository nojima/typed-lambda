{-# LANGUAGE OverloadedStrings #-}
module Predefined (Function(..), functions) where

import qualified Identifier
import           Type (TypeScheme(..))
import qualified Type
import           Value (Value, RuntimeError(..))
import qualified Value

data Function =
    Function
        { name       :: Identifier.Identifier
        , typeScheme :: TypeScheme
        , function   :: Value -> Either RuntimeError Value
        }

functions :: [Function]
functions =
    [ Function
        "head"
        (ForAll ["a"]
            (Type.Function (Type.List (Type.Var "a")) (Type.Var "a")))
        pdHead
    ]

pdHead :: Value -> Either RuntimeError Value
pdHead value =
    case value of
        Value.List elements ->
            if null elements then
                Left $ RuntimeError "head: the given list is empty"
            else
                Right $ head elements
        _ ->
            Left $ RuntimeError "head: the given value is not a list"
