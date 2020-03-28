module Term where

data Term
    = True
    | False
    | If Term Term Term
    | Zero
    | Succ Term
    | Pred Term
    | IsZero Term
    deriving (Show, Eq)
