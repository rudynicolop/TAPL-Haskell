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

data NoType
data NatType
data BoolType
data ArrowType a b = ArrowType a b

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
  XVar :: Id -> t -> Expr c n b t
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

type UnTypedExpr = Expr Empty NoType NoType NoType

-- data Type = TNat | TBool | TArrow Type Type
--
-- data UnTyped = UnTyped deriving (Eq)
-- data TyNat = TyNat deriving (Eq)
-- data TyBool = TyBool deriving (Eq)
-- data TyArrow t1 t2 = TyArrow t1 t2 deriving (Eq)
--
--
-- data Expr k n b t where
--   XNat :: VNat -> Expr k n b n
--   XBool :: VBool -> Expr k n b b
--   XVar :: Id -> Expr k n b t
--   XAdd :: Expr k n b n -> Expr k n b n -> Expr k n b n
--   XAnd :: Expr k n b b -> Expr k n b b -> Expr k n b b
--   XCond :: Expr k n b b -> Expr k n b t -> Expr k n b t -> Expr k n b t
--   XLam :: SimpType t' => Id -> t' -> Expr k n b t -> Expr k n b (k t' t)
--   XApp :: SimpType t' => Expr k n b (k t' t) -> Expr k n b t' -> Expr k n b t
--
-- -- untyped programs
-- type UExpr = Expr EmptyArrow UnTyped UnTyped UnTyped
--
-- -- typed programs
-- type TExpr t = Expr TyArrow TyNat TyBool t
