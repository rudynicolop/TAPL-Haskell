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

module Hypothetical where

import           AST

{-Below is a failed experiment into Haskell's type system.

  Hence this a "hypothetical" expression type for the
  simply-typed lambda calculus. The idea, that the types in the
  simply-typed lambda calculus are reflected in Haskell's type
  system was interesting to pursue, but a type-checker for it would have
  been infinite, because there are countably-infinite many types.

  We begin with the base types, and construct more types with "->",
  where the subsuquent layers are permutations of types defined in
  previous layers.
  -----------------------------------
    Nat
  -----------------------------------
    Bool
  -----------------------------------
    Nat -> Nat
    Nat -> Bool
    Bool -> Nat
    Bool -> Bool
  -----------------------------------
    Nat -> (Nat -> Nat)
    Nat -> (Nat -> Bool)
    Nat -> (Bool -> Nat)
    Nat -> (Bool -> Bool)
    Bool -> (Nat -> Nat)
    Bool -> (Nat -> Bool)
    Bool -> (Bool -> Nat)
    Bool -> (Bool -> Bool)
    (Nat -> Nat) -> Nat
    (Nat -> Bool) -> Nat
    (Bool -> Nat) -> Nat
    (Bool -> Bool) -> Nat
    (Nat -> Nat) -> Bool
    (Nat -> Bool) -> Bool
    (Bool -> Nat) -> Bool
    (Bool -> Bool) -> Bool
    (Nat -> Nat) -> (Nat -> Nat)
    (Nat -> Nat) -> (Nat -> Bool)
    (Nat -> Nat) -> (Bool -> Nat)
    (Nat -> Nat) -> (Bool -> Bool)
    (Nat -> Bool) -> (Nat -> Nat)
    (Nat -> Bool) -> (Nat -> Bool)
    (Nat -> Bool) -> (Bool -> Nat)
    (Nat -> Bool) -> (Bool -> Bool)
    (Bool -> Nat) -> (Nat -> Nat)
    (Bool -> Nat) -> (Nat -> Bool)
    (Bool -> Nat) -> (Bool -> Nat)
    (Bool -> Nat) -> (Bool -> Bool)
    (Bool -> Bool) -> (Nat -> Nat)
    (Bool -> Bool) -> (Nat -> Bool)
    (Bool -> Bool) -> (Bool -> Nat)
    (Bool -> Bool) -> (Bool -> Bool)
  -----------------------------------
    ...
  -----------------------------------
  This process stops until a fixpoint of the type-space is reached,
  but this is an infinite process.

  c=constraint a=arrowConstructor n=natType b=boolType t=thisType -}

type family Arrow t' t where
  Arrow NoType NoType = NoType
  Arrow NatType NoType = NoType
  Arrow BoolType NoType = NoType
  Arrow (ArrowType t' t) NoType = NoType
  Arrow Type Type = Type
  Arrow a b = ArrowType a b

class SimpType t
instance SimpType NatType
instance SimpType BoolType
instance (SimpType t', SimpType t) => SimpType (ArrowType t' t)

data NoType = NoType
  deriving (Eq)

data NatType = NatType
  deriving (Eq)

data BoolType = BoolType
  deriving (Eq)

data ArrowType a b = ArrowType a b
  deriving (Eq)

data HyExpr c n b t where
  XNat :: Nat -> HyExpr c n b n
  XBool :: Bul -> HyExpr c n b b
  XVar :: Id -> HyExpr c n b t
  XAdd :: HyExpr c n b n -> HyExpr c n b n -> HyExpr c n b n
  XCond :: HyExpr c n b b -> HyExpr c n b t -> HyExpr c n b t -> HyExpr c n b t
  XLam :: SimpType t' => Id -> t' -> HyExpr c n b t -> HyExpr c n b (Arrow t' t)
  XApp :: c t' => HyExpr c n b (Arrow t' t) -> HyExpr c n b t' -> HyExpr c n b t

class Empty t
instance Empty t

-- hypothetical untyped expression type
type HyUnTypedExpr = HyExpr Empty NoType NoType NoType

-- hypothetical typed expressions
type HyTypedExpr t = HyExpr SimpType NatType BoolType t
