module Term (Term(..)) where

import Identifier (Identifier)
import Type (Type)

data Term
    = Bool Bool
    | Variable Identifier
    | Lambda Identifier Type Term
    | Apply Term Term
    deriving (Show, Eq)
