{-# LANGUAGE UndecidableInstances #-}

module AST where

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
data T e = Ty Type e
  deriving (Eq)

type BExpr = Expr B

type TExpr = Expr T

data Value =
  VNat Nat
  | VBul Bul
  | VLam Id Type TExpr
