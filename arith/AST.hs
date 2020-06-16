module AST where

(|>) x f = f x

data Term =
    TTrue
    | TFalse
    | Zero
    | IsZero Term
    | Succ Term
    | Pred Term
    | IfThenElse Term Term Term
    deriving Show

genNatTerm :: Integer -> Term
genNatTerm 0 = Zero
genNatTerm n = Succ $ genNatTerm (n-1)

data Nat = Z | S Nat deriving Show

data VB = VTrue | VFalse deriving Show

data Val =
  VNum Nat
  | VBool VB
  deriving Show
