module Type (Type(..)) where

data Type
    = Bool
    | Function Type Type
    deriving (Show, Eq)
