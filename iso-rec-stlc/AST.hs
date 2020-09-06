{-# LANGUAGE UndecidableInstances #-}

module AST where

type Id = String

data Type =
  TUnit
  | TFun Type Type
  | TVar Id
  | TRec Id Type
  deriving (Eq)

instance Show Type where
  show TUnit      = "unit"
  show (TFun a b) = "(" ++ (show a) ++ " -> " ++ (show b) ++ ")"
  show (TVar x)   = x
  show (TRec x t) = "(μ " ++ x ++ ". " ++ (show t) ++ ")"

data Expr k =
  EUnit
  | EVar (k Id)
  | EFun Id Type (k (Expr k))
  | EApp (k (Expr k)) (k (Expr k))
  | EFold Id Type (k (Expr k))
  | EUnfold Id Type (k (Expr k))

instance Eq (t (Expr t)) => Eq (Expr t) where
    (==) = (==)

-- bare/blank annotation
data B e = B e
  deriving (Eq)

instance Show e => Show (B e) where
  show (B e) = show e

-- type annotation
data T e = T Type e
  deriving (Eq)

instance Show e => Show (T e) where
  show (T _ e) = show e

class Annotation t where
  ge :: t (Expr t) -> Expr t
  gx :: t (Id) -> Id

instance Annotation B where
  ge (B e) = e
  gx (B x) = x

instance Annotation T where
  ge (T _ e) = e
  gx (T _ x) = x

instance (Annotation t, Show (t (Expr t))) => Show (Expr t) where
  show EUnit = "()"
  show (EVar x) = gx x
  show (EFun x t e) = "(fun " ++ x ++ ": " ++ (show t) ++ ". " ++ (show e) ++ ")"
  show (EApp e1 e2) = "(" ++ (show e1) ++ " " ++ (show e2) ++ ")"
  show (EFold x t e) = "(fold [" ++ (show $ TRec x t) ++ "] " ++ (show e) ++ ")"
