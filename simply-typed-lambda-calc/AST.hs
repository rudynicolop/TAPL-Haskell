{-# LANGUAGE UndecidableInstances #-}

module AST where

(|>) x f = f x

type Id = String

data Nat = Z | S Nat
  deriving (Eq, Show)

data Bul = T | F
  deriving (Eq, Show)

data Type =
  TNat
  | TBul
  | TArrow Type Type
  deriving (Eq, Show)

data Expr t =
  ENat Nat
  | EBul Bul
  | EVar (t Id)
  | ENot (t (Expr t))
  | EAdd (t (Expr t)) (t (Expr t))
  | EMul (t (Expr t)) (t (Expr t))
  | ESub (t (Expr t)) (t (Expr t))
  | EEq (t (Expr t)) (t (Expr t))
  | ELe (t (Expr t)) (t (Expr t))
  | EAnd (t (Expr t)) (t (Expr t))
  | EOr (t (Expr t)) (t (Expr t))
  | ECond (t (Expr t)) (t (Expr t)) (t (Expr t))
  | ELam Id Type (t (Expr t))
  | EApp (t (Expr t)) (t (Expr t))

instance Eq (t (Expr t)) => Eq (Expr t) where
    (==) = (==)

-- bare annotation
data B e = B e
  deriving (Eq)

-- type annotation
data Ty e = Ty Type e
  deriving (Eq)

type BExpr = Expr B

type TExpr = Expr Ty

class Annotation t where
  ge :: t (Expr t) -> Expr t
  gx :: t (Id) -> Id

instance Annotation B where
  ge (B e) = e
  gx (B x) = x

instance Annotation Ty where
  ge (Ty _ e) = e
  gx (Ty _ x) = x

data Value =
  VNat Nat
  | VBul Bul
  | VLam Id Type TExpr
