module AST where

type Id = String

data VNat = Z | S VNat deriving (Eq)

data VBool = VTrue | VFalse deriving (Eq)

data Type =
  TNat
  | TBool
  | TArrow Type Type

data Expr t =
  ENat VNat
  | EBool VBool
  | EVar (t Id)
  | EAdd (t (Expr t)) (t (Expr t))
  | EMul (t (Expr t)) (t (Expr t))
  | ESub (t (Expr t)) (t (Expr t))
  | EEq (t (Expr t)) (t (Expr t))
  | ELeq (t (Expr t)) (t (Expr t))
  | ENot (t (Expr t))
  | EAnd (t (Expr t)) (t (Expr t))
  | EOr (t (Expr t)) (t (Expr t))
  | ECond (t (Expr t)) (t (Expr t)) (t (Expr t))
  | ELam Id Type (t (Expr t))
  | EApp (t (Expr t)) (t (Expr t))

data Bare e = Bare e

data Typed e = Typed Type e

type BareExpr = Expr Bare

type TypedExpr = Expr Typed
