-- {-# LANGUAGE StandaloneDeriving   #-}
-- {-# LANGUAGE TypeSynonymInstances #-}

module AST where

type Id = String
-- deriving instance (Eq Id)
-- deriving instance (Show Id)


data VNat = Z | S VNat
  -- deriving (Eq, Show)

data VBool = VTrue | VFalse
  -- deriving (Eq, Show)

data Type =
  TNat
  | TBool
  | TArrow Type Type
  deriving (Eq, Show)

data Expr t =
  ENat VNat
  | EBool VBool
  | EVar (t Id)
  | ENot (t (Expr t))
  | EAdd (t (Expr t)) (t (Expr t))
  | EMul (t (Expr t)) (t (Expr t))
  | ESub (t (Expr t)) (t (Expr t))
  | EEq (t (Expr t)) (t (Expr t))
  | ELeq (t (Expr t)) (t (Expr t))
  | EAnd (t (Expr t)) (t (Expr t))
  | EOr (t (Expr t)) (t (Expr t))
  | ECond (t (Expr t)) (t (Expr t)) (t (Expr t))
  | ELam Id Type (t (Expr t))
  | EApp (t (Expr t)) (t (Expr t))
  -- deriving (Eq, Show)

-- bare annotation
data B e = B e
  -- deriving (Eq, Show)

-- type annotation
data T e = T Type e
  -- deriving (Eq, Show)

type BExpr = Expr B

type TExpr = Expr T
