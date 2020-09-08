{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module AST where

(|>) x f = f x

type Id = String

data Nat = Z | S Nat
  deriving (Eq)

toInt Z     = 0
toInt (S n) = 1 + (toInt n)

instance Show Nat where
  show n = show $ toInt n

data Bul = T | F
  deriving (Eq)

instance Show Bul where
  show T = "true"
  show F = "false"

data Type =
  TNat
  | TBul
  | TArrow Type Type
  deriving (Eq)

instance Show Type where
  show TNat           = "Nat"
  show TBul           = "Bool"
  show (TArrow t1 t2) = "(" ++ (show t1) ++ " -> " ++ (show t2) ++ ")"

data BOp =
  EAdd
  | EMul
  | ESub
  | EEq
  | ELe
  | EAnd
  | EOr
  deriving (Eq)

instance Show BOp where
  show EAdd = "+"
  show EMul = "*"
  show EEq  = "="
  show ELe  = "<"
  show EAnd = "&"
  show EOr  = "|"

data Expr t =
  ENat Nat
  | EBul Bul
  | EVar (t Id)
  | ENot (t (Expr t))
  | EBOp BOp (t (Expr t)) (t (Expr t))
  | ECond (t (Expr t)) (t (Expr t)) (t (Expr t))
  | ELam Id Type (t (Expr t))
  | EApp (t (Expr t)) (t (Expr t))

instance Eq (t (Expr t)) => Eq (Expr t) where
    (==) = (==)

-- bare annotation
data B e = B e
  deriving (Eq)

instance Show e => Show (B e) where
  show (B e) = show e

-- type annotation
data Ty e = Ty Type e
  deriving (Eq)

instance Show e => Show (Ty e) where
  show (Ty _ e) = show e

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

instance (Annotation t, Show (t (Expr t))) => Show (Expr t) where
  show (ENat n) = show n
  show (EBul b) = show b
  show (EVar x) = x |> gx
  show (ENot e) = "(!" ++ (show e) ++ ")"
  show (EBOp op e1 e2) = "(" ++ (show e1) ++ " " ++ (show op) ++ " " ++ (show e2) ++ ")"
  show (ECond e1 e2 e3) = "(if " ++ (show e1) ++ " then " ++ (show e2) ++ " else " ++ (show e3) ++ ")"
  show (ELam x t e) = "(fun " ++ x ++ " : " ++ (show t) ++ " => " ++ (show e) ++ ")"
  show (EApp e1 e2) = "(" ++ (show e1) ++ " " ++ (show e2) ++ ")"
