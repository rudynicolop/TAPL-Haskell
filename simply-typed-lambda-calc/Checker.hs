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

type Gamma = M.Map Id Type

data TProg = PNat (TExpr TyNat) | PBool (TExpr TyBool) | PArrow (TExpr (TyArrow Type Type))

type Result = Maybe TProg

check :: Gamma -> UExpr -> Result
check _ (XNat n)  = do return $ PNat $ XNat n
check _ (XBool b) = do return $ PBool $ XBool b
check g (XVar x)  = do
  t <- M.lookup x g
  case t of
    TNat TyNat             -> do return $ PNat $ XVar x
    TBool TyBool           -> do return $ PBool $ XVar x
    TArrow (TyArrow t1 t2) -> do return $ PArrow $ XVar x
check g (XAdd e1 e2) = do
  e1' <- check g e1
  e2' <- check g e2
  case (e1',e2') of
    (PNat p1, PNat p2) -> do return $ PNat $ XAdd p1 p2
    _                  -> Nothing
