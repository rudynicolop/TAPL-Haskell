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

data Expr c k n b t where
  XNat :: VNat -> Expr c k n b n
  XBool :: VBool -> Expr c k n b b
  XVar :: Id -> Expr c k n b t
  XAdd :: Expr c k n b n -> Expr c k n b n -> Expr c k n b n
  XAnd :: Expr c k n b b -> Expr c k n b b -> Expr c k n b b
  XCond :: Expr c k n b b -> Expr c k n b t -> Expr c k n b t -> Expr c k n b t
  XLam :: c t' => Id -> t' -> Expr c k n b t -> Expr c k n b (k t' t)
  XApp :: c t' => Expr c k n b (k t' t) -> Expr c k n b t' -> Expr c k n b t
  -- deriving (Eq)

-- untyped programs
type UExpr = Expr Empty EmptyArrow UnTyped UnTyped UnTyped
  -- deriving (Eq)

-- typed programs
type TExpr t = Expr SimpType TyArrow TyNat TyBool t
  -- deriving (Eq)
