module AST where

type Id = String

data Type = TNat | TBool

data VBool = VTrue | VFalse

data VNat = Z | S VNat

data Expr =
  EBool VBool
  | ENat VNat
  | ECond Expr Expr Expr
  | EVar Id
  | ELam Id Expr
  | EApp Expr
