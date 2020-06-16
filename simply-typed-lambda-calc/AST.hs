module AST where

type Id = String

data Type = TNat | TBool | TArrow Type Type

data VBool = VTrue | VFalse

data VNat = Z | S VNat

data NExpr =
  NNat VNat
  | NAdd NExpr NExpr
  | NSub NExpr NExpr
  | NMul NExpr NExpr

data BExpr =
  BBool VBool
  | BNot VBool
  | BAnd VBool VBool
  | BOr VBool VBool
  | BEq NExpr NExpr
  | BLeq NExpr NExpr

data Expr =
  ENat NExpr
  | Bool BExpr
  | ECond BExpr NExpr NExpr
  | EVar Id
  | ELam Id Type Expr
  | EApp Expr Expr
