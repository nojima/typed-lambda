{-# LANGUAGE OverloadedStrings #-}
module Predefined (Function(..), functions) where

import qualified Identifier
import           Type (TypeScheme(..))
import qualified Type
import           Value (Value, RuntimeError(..))
import qualified Value
import           Data.Vector ((!))

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
    , Function
        "tail"
        (ForAll ["a"]
            (Type.Function (Type.List (Type.Var "a")) (Type.List (Type.Var "a"))))
        pdTail
    , Function
        "null"
        (ForAll ["a"]
            (Type.Function (Type.List (Type.Var "a")) Type.Bool))
        pdNull
    , Function
        "cons"
        (ForAll ["a"]
            (Type.Function (Type.Var "a") (Type.Function (Type.List (Type.Var "a")) (Type.List (Type.Var "a")))))
        pdCons1
    , Function
        "fst"
        (ForAll ["a", "b"]
            (Type.Function (Type.Tuple [Type.Var "a", Type.Var "b"]) (Type.Var "a")))
        pdFst
    , Function
        "snd"
        (ForAll ["a", "b"]
            (Type.Function (Type.Tuple [Type.Var "a", Type.Var "b"]) (Type.Var "b")))
        pdSnd
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

pdTail :: Value -> Either RuntimeError Value
pdTail value =
    case value of
        Value.List elements ->
            if null elements then
                Left $ RuntimeError "tail: the given list is empty"
            else
                Right $ Value.List (tail elements)
        _ ->
            Left $ RuntimeError "tail: the given value is not a list"

pdNull :: Value -> Either RuntimeError Value
pdNull value =
    case value of
        Value.List elements ->
            Right $ Value.Bool (null elements)
        _ ->
            Left $ RuntimeError "null: the given value is not a list"

pdCons1 :: Value -> Either RuntimeError Value
pdCons1 value =
    Right $ Value.NativeFunction "cons" (pdCons2 value)

pdCons2 :: Value -> Value -> Either RuntimeError Value
pdCons2 value1 value2 =
    case value2 of
        Value.List elements ->
            Right $ Value.List (value1:elements)
        _ ->
            Left $ RuntimeError "cons: the 2nd argument is not a list"

pdFst :: Value -> Either RuntimeError Value
pdFst value =
    case value of
        Value.Tuple elements ->
            Right $ elements ! 0
        _ ->
            Left $ RuntimeError "fst: the given value is not a tuple"

pdSnd :: Value -> Either RuntimeError Value
pdSnd value =
    case value of
        Value.Tuple elements ->
            Right $ elements ! 1
        _ ->
            Left $ RuntimeError "snd: the given value is not a tuple"
