{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}

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
  | TCond TBExpr TExpr TExpr
  | TVar Id
  | TLam Id Type TExpr
  | TApp TExpr TExpr

data Expr n b =
  EXNat VNat
  | EXBool VBool
  | EXCond (b (Expr n b)) (Expr n b) (Expr n b)
  | EXAdd (n (Expr n b)) (n (Expr n b))

class Empty e
instance Empty e

data ENat = ENat VNat
data EAdd a = EAdd a a

class IsNatExpr a
instance IsNatExpr ENat
instance IsNatExpr a => IsNatExpr (EAdd a)

data EBool = EBool VBool
data EAnd a = EAnd a a

class IsBoolExpr a
instance IsBoolExpr EBool
instance IsBoolExpr a => IsBoolExpr (EAnd a)

data TyNat = TyNat
data TyBool = TyBool

data TyArrow t1 t2 = TyArrow t1 t2

data GATExpr t n b where
  GATNat :: VNat -> GATExpr TyNat n b
  GATBool :: VBool -> GATExpr TyBool n b
  GATAdd :: n e => e -> e -> GATExpr TyNat n b
  GATAnd :: b e => e -> e -> GATExpr TyBool n b
  GATCond :: b e1 => e1 -> e2 -> e2 -> GATExpr t n b

type UGATExpr t = GATExpr t Empty Empty

type TGATExpr t = GATExpr t IsNatExpr IsBoolExpr
