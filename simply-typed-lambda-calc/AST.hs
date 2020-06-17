{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}

module AST where

type Id = String

data Type = TNat | TBool | TArrow Type Type

data VBool = VTrue | VFalse

data VNat = Z | S VNat

data EmptyArrow a b = EmptyArrow

data UnTyped = UnTyped
data TyNat = TyNat
data TyBool = TyBool
data TyArrow t1 t2 = TyArrow t1 t2

class Empty t
instance Empty t

class SimpType t
instance SimpType TyNat
instance SimpType TyBool
instance (SimpType t1, SimpType t2) => SimpType (TyArrow t1 t2)

data Expr c k n b t where
  XNat :: VNat -> Expr c k n b n
  XBool :: VBool -> Expr c k n b b
  XAdd :: Expr c k n b n -> Expr c k n b n -> Expr c k n b n
  XAnd :: Expr c k n b b -> Expr c k n b b -> Expr c k n b b
  XCond :: Expr c k n b b -> Expr c k n b t -> Expr c k n b t -> Expr c k n b t
  XLam :: c t' => Id -> t' -> Expr c k n b t -> Expr c k n b (k t' t)
  XApp :: c t' => Expr c k n b (k t' t) -> Expr c k n b t' -> Expr c k n b t

-- untyped programs
type UExpr = Expr Empty EmptyArrow UnTyped UnTyped UnTyped

-- typed programs
type TExpr t = Expr SimpType TyArrow TyNat TyBool t
