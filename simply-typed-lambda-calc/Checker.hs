{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}

module Checker where

import           AST
import           Data.Map.Strict as M

data Type where
  TNat :: TyNat -> Type
  TBool :: TyBool -> Type
  TArrow :: TyArrow Type Type -> Type
  deriving (Eq)

type Gamma = M.Map Id Type

data TProg =
  PNat (TExpr TyNat)
  | PBool (TExpr TyBool)
  | PArrow Type Type (TExpr (TyArrow Type Type))
  -- deriving (Eq)

type Result = Maybe TProg

check :: Gamma -> UExpr -> Result
check _ (XNat n)  = do return $ PNat $ XNat n
check _ (XBool b) = do return $ PBool $ XBool b
check g (XVar x)  = do
  t <- M.lookup x g
  case t of
    TNat TyNat             -> do return $ PNat $ XVar x
    TBool TyBool           -> do return $ PBool $ XVar x
    TArrow (TyArrow t1 t2) -> do return $ PArrow t1 t2 $ XVar x
check g (XAdd e1 e2) = do
  e1' <- check g e1
  e2' <- check g e2
  case (e1', e2') of
    (PNat p1, PNat p2) -> do return $ PNat $ XAdd p1 p2
    _                  -> Nothing
check g (XAnd e1 e2) = do
  e1' <- check g e1
  e2' <- check g e2
  case (e1', e2') of
    (PBool p1, PBool p2) -> do return $ PBool $ XAnd p1 p2
    _                    -> Nothing
check g (XCond e1 e2 e3) = do
  e1' <- check g e1
  e2' <- check g e2
  e3' <- check g e3
  checkcond e1' e2' e3'
  where
    checkcond :: TProg -> TProg -> TProg -> Result
    checkcond  (PBool p1) (PBool p2) (PBool p3) = do return $ PBool $ XCond p1 p2 p3
    checkcond  (PBool p1) (PNat p2) (PNat p3) = do return $ PNat $ XCond p1 p2 p3
    checkcond  (PBool p1) (PArrow t21 t22 p2) (PArrow t31 t32 p3)
      | t21 == t31 && t22 == t32 = do return $ PArrow t21 t22 $ XCond p1 p2 p3
      | otherwise = Nothing
-- check g (XLam x t e) = do
--   e' <- check (M.insert x t g) e
--   case e' of
--     PBool p        -> do return $ PArrow t TyBool (XLam x t p)
--     PNat p         -> do return $ PArrow t TyNat (XLam x t p)
--     PArrow t1 t2 p -> do return $ PArrow t (TArrow t1 t2) (XLam x t p)
