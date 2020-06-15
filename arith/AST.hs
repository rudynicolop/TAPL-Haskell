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

data Nat = Z | S Nat deriving Show

data VB = VTrue | VFalse deriving Show

data Val =
  VNum Nat
  | VBool VB
  deriving Show
