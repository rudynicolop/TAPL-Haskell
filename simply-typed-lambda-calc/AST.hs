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
{-# LANGUAGE TypeFamilies              #-}

module AST where

type Id = String

data VBool = VTrue | VFalse deriving (Eq)

data VNat = Z | S VNat deriving (Eq)

data NoType = NoType
  deriving (Eq)

data NatType = NatType
  deriving (Eq)

data BoolType = BoolType
  deriving (Eq)

data ArrowType a b = ArrowType a b
  deriving (Eq)

type family Arrow t' t :: * where
  Arrow NoType NoType = NoType
  Arrow NatType NoType = NoType
  Arrow BoolType NoType = NoType
  Arrow (ArrowType t' t) NoType = NoType
  Arrow a b = ArrowType a b

-- c=constraint a=arrowConstructor n=natType b=boolType t=thisType
data Expr c n b t where
  XNat :: VNat -> Expr c n b n
  XBool :: VBool -> Expr c n b b
  XVar :: Id -> Expr c n b t
  XAdd :: Expr c n b n -> Expr c n b n -> Expr c n b n
  XCond :: Expr c b n b -> Expr c b n t -> Expr c b n t -> Expr c b n t
  XLam :: c t' => Id -> t' -> Expr c b n t -> Expr c b n (Arrow t' t)
  XApp :: c t' => Expr c n b (Arrow t' t) -> Expr c n b t' -> Expr c n b t

class Empty t
instance Empty t

class SimpType t
instance SimpType NatType
instance SimpType BoolType
instance (SimpType t', SimpType t) => SimpType (ArrowType t' t)

-- untyped expressions
type UnTypedExpr = Expr Empty NoType NoType NoType

-- typed expressions
type TypedExpr t = Expr SimpType NatType BoolType t
