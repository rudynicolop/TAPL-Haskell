{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}

module AST where

type Id = String

data Type = TNat | TBool | TArrow Type Type

data VBool = VTrue | VFalse

data VNat = Z | S VNat

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

data ECond a b = ECond a b b

data ELam e = ELam Id Type e

class IsArrowExpr e
instance IsArrowExpr (ELam e)

data TyNat = TyNat
data TyBool = TyBool

data TyArrow t1 t2 = TyArrow t1 t2

data GExpr t n b where
  GNat :: VNat -> GExpr n n b
  GBool :: VBool -> GExpr n n b
  GAdd :: n e => EAdd e -> GExpr n n b
  GAnd :: b e => EAnd e -> GExpr b n b
  GCond :: (b u, t v) =>  ECond u v -> GExpr t n b

type UGExpr t = GExpr t Empty Empty

type TGExpr t = GExpr t IsNatExpr IsBoolExpr
