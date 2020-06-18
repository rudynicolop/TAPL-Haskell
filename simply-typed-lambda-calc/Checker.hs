-- {-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE RankNTypes                #-}

module Checker where

import           AST
import           Data.Map.Strict as M

data Type where
  TNat :: NatType -> Type
  TBool :: BoolType -> Type
  TArrow :: ArrowType Type Type -> Type
  deriving (Eq)

type Gamma = M.Map Id Type

type Result t = Maybe (TypedExpr t)

class TypeCheck t where
  check :: Gamma -> UnTypedExpr -> Result t

instance TypeCheck NatType where
  check :: Gamma -> UnTypedExpr -> Result NatType
  check _ (XNat n) = do return $ XNat n
  check g (XVar x) = do
    t <- M.lookup x g
    case t of
      TNat NatType -> do return $ XVar x
      _            -> Nothing
  check g (XAdd e1 e2) = do
    e1' <- check g e1
    e2' <- check g e2
    return $ XAdd e1' e2'

--
-- -- data TypeWrapper = TypeWrapper Type
--
-- -- * -> * -> *, where the last * is a Type
--
-- class TypeArrow a b where
--   type TA = * -> * -> Type
--
-- instance TypeArrow a b where
--   type TA a b = Type
--
-- -- wrapped types, toothless constraints
-- type WExpr = Expr TypeArrow Type Type Type
--
--
-- data TProg =
--   PNat (TExpr TyNat)
--   | PBool (TExpr TyBool)
--   | PArrow Type Type (TExpr (TyArrow Type Type))
--
-- class WrapType t where
--   wrapt :: t -> Type
--
-- instance WrapType TyNat where
--   wrapt :: TyNat -> Type
--   wrapt TyNat = TNat TyNat
--
-- instance WrapType TyBool where
--   wrapt :: TyBool -> Type
--   wrapt TyBool = TBool TyBool
--
-- instance (WrapType t1, WrapType t2) => WrapType (TyArrow t1 t2) where
--   wrapt :: (TyArrow t1 t2) -> Type
--   wrapt (TyArrow t1 t2) = TArrow $ TyArrow (wrapt t1) (wrapt t2)
--
-- class Wrap t where
--   wrap :: (TExpr t) -> WExpr
--
-- instance Wrap Type where
--   wrap :: (TExpr Type) -> WExpr
--   wrap (XVar x)         = XVar x
--   wrap (XCond e1 e2 e3) = XCond (wrap e1) (wrap e2) (wrap e2)
--   wrap (XLam x t e)     = XLam x (wrapt t) (wrap e)
--
-- instance Wrap TyNat where
--   wrap :: TExpr TyNat -> WExpr
--   wrap (XNat n)         = XNat n
--   wrap (XVar x)         = XVar x
--   wrap (XAdd e1 e2)     = XAdd (wrap e1) (wrap e2)
--   wrap (XCond e1 e2 e3) = XCond (wrap e1) (wrap e2) (wrap e3)
--
-- instance Wrap TyBool where
--   wrap :: TExpr TyBool -> WExpr
--   wrap (XBool b)        = XBool b
--   wrap (XVar x)         = XVar x
--   wrap (XAnd e1 e2)     = XAnd (wrap e1) (wrap e2)
--   wrap (XCond e1 e2 e3) = XCond (wrap e1) (wrap e2) (wrap e3)
--
-- instance (Wrap t1, Wrap t2) => Wrap (TyArrow t1 t2) where
--   wrap :: TExpr (TyArrow t1 t2) -> WExpr
--   wrap (XVar x)         = (XVar x)
--   wrap (XCond e1 e2 e3) = XCond (wrap e1) (wrap e2) (wrap e3)
--   wrap (XLam x TyNat e) = XLam x (TNat TyNat) (wrap e)
--
-- wrapNatExpr :: TExpr TyNat -> WExpr
-- wrapNatExpr (XNat n)     = XNat n
-- wrapNatExpr (XVar x)     = XVar x
-- wrapNatExpr (XAdd e1 e2) = XAdd (wrapNatExpr e1) (wrapNatExpr e2)
-- wrapNatExpr (XCond e1 e2 e3) = XCond (wrapBoolExpr e1) (wrapNatExpr e2) (wrapNatExpr e3)
--
-- wrapBoolExpr :: TExpr TyBool -> WExpr
-- wrapBoolExpr (XBool b)    = XBool b
-- wrapBoolExpr (XVar x) = XVar x
-- wrapBoolExpr (XAnd e1 e2) = XAnd (wrapBoolExpr e1) (wrapBoolExpr e2)
-- wrapBoolExpr (XCond e1 e2 e3) = XCond (wrapBoolExpr e1) (wrapBoolExpr e2) (wrapBoolExpr e3)
--
-- wrapArrowExpr :: TExpr (TyArrow a b) -> WExpr
-- wrapArrowExpr (XVar x) = XVar x
-- wrapArrowExpr (XCond e1 e2 e3) = XCond (wrapBoolExpr e1) (wrapArrowExpr e2) (wrapArrowExpr e3)
-- -- wrapArrowExpr (XLam x TyNat e) = XLam x (TNat TyNat) e
--
-- type Result = Maybe TProg
--
-- check :: Gamma -> UExpr -> Result
-- check _ (XNat n)  = do return $ PNat $ XNat n
-- check _ (XBool b) = do return $ PBool $ XBool b
-- check g (XVar x)  = do
--   t <- M.lookup x g
--   case t of
--     TNat TyNat             -> do return $ PNat $ XVar x
--     TBool TyBool           -> do return $ PBool $ XVar x
--     TArrow (TyArrow t1 t2) -> do return $ PArrow t1 t2 $ XVar x
-- check g (XAdd e1 e2) = do
--   e1' <- check g e1
--   e2' <- check g e2
--   case (e1', e2') of
--     (PNat p1, PNat p2) -> do return $ PNat $ XAdd p1 p2
--     _                  -> Nothing
-- check g (XAnd e1 e2) = do
--   e1' <- check g e1
--   e2' <- check g e2
--   case (e1', e2') of
--     (PBool p1, PBool p2) -> do return $ PBool $ XAnd p1 p2
--     _                    -> Nothing
-- check g (XCond e1 e2 e3) = do
--   e1' <- check g e1
--   e2' <- check g e2
--   e3' <- check g e3
--   checkcond e1' e2' e3'
--   where
--     checkcond :: TProg -> TProg -> TProg -> Result
--     checkcond  (PBool p1) (PBool p2) (PBool p3) = do return $ PBool $ XCond p1 p2 p3
--     checkcond  (PBool p1) (PNat p2) (PNat p3) = do return $ PNat $ XCond p1 p2 p3
--     checkcond  (PBool p1) (PArrow t21 t22 p2) (PArrow t31 t32 p3)
--       | t21 == t31 && t22 == t32 = do return $ PArrow t21 t22 $ XCond p1 p2 p3
--       | otherwise = Nothing
-- -- check g (XLam x t e) = do
-- --   e' <- check (M.insert x t g) e
-- --   case e' of
-- --     PBool p        -> do return $ PArrow t (TBool TyBool) (XLam x t p)
-- --     PNat p         -> do return $ PArrow t (TNat TyNat) (XLam x t p)
-- --     PArrow t1 t2 p -> do return $ PArrow t (TArrow $ TyArrow t1 t2) (XLam x t p)
