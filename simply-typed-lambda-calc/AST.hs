module AST where

type Id = String

data Type = TNat | TBool | TArrow Type Type

data VBool = VTrue | VFalse

data VNat = Z | S VNat

data UExpr =
  UNat VNat
  | UAdd UExpr UExpr
  | USub UExpr UExpr
  | UMul UExpr UExpr
  | UBool VNat
  | UNot UExpr
  | UAnd UExpr UExpr
  | UOr UExpr UExpr
  | UEq UExpr UExpr
  | ULeq UExpr UExpr
  | UCond UExpr UExpr UExpr
  | UVar Id
  | ULam Id Type UExpr
  | UApp UExpr UExpr

data TNExpr =
  NNat VNat
  | NAdd TNExpr TNExpr
  | NSub TNExpr TNExpr
  | NMul TNExpr TNExpr

data TBExpr =
  BBool VBool
  | BNot TBExpr
  | BAnd TBExpr TBExpr
  | BOr TBExpr TBExpr
  | BEq TNExpr TNExpr
  | BLeq TNExpr TNExpr

data TExpr =
  TENat TNExpr
  | TEBool TBExpr
  | TCond TBExpr TNExpr TNExpr
  | TVar Id
  | TLam Id Type TExpr
  | TApp TExpr TExpr
