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

-- data Type = TNat | TBool | TArrow Type Type

data VBool = VTrue | VFalse deriving (Eq)

data VNat = Z | S VNat deriving (Eq)

data EmptyArrow a b = EmptyArrow deriving (Eq)

data UnTyped = UnTyped deriving (Eq)
data TyNat = TyNat deriving (Eq)
data TyBool = TyBool deriving (Eq)
data TyArrow t1 t2 = TyArrow t1 t2 deriving (Eq)

class Empty t
instance Empty t

class SimpType t
instance SimpType TyNat
instance SimpType TyBool
instance (SimpType t1, SimpType t2) => SimpType (TyArrow t1 t2)

data Expr k n b t where
  XNat :: VNat -> Expr k n b n
  XBool :: VBool -> Expr k n b b
  XVar :: Id -> Expr k n b t
  XAdd :: Expr k n b n -> Expr k n b n -> Expr k n b n
  XAnd :: Expr k n b b -> Expr k n b b -> Expr k n b b
  XCond :: Expr k n b b -> Expr k n b t -> Expr k n b t -> Expr k n b t
  XLam :: SimpType t' => Id -> t' -> Expr k n b t -> Expr k n b (k t' t)
  XApp :: SimpType t' => Expr k n b (k t' t) -> Expr k n b t' -> Expr k n b t

-- untyped programs
type UExpr = Expr EmptyArrow UnTyped UnTyped UnTyped

-- typed programs
type TExpr t = Expr TyArrow TyNat TyBool t
