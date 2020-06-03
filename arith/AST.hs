module AST where

data Term =
    TTrue
    | TFalse
    | Zero
    | IsZero Term
    | Succ Term
    | Pred Term
    | IfThenElse Term Term Term
    deriving Show

data Val =
  VNum Int
  | VBool Bool
  deriving Show
