{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Eval where

import           AST
import qualified Control.Monad.Trans.Class      as MT
import qualified Control.Monad.Trans.Fix        as FIX
import qualified Control.Monad.Trans.State.Lazy as ST
import qualified Data.Set                       as S

-- set of free variables in an expression
fv :: TExpr -> S.Set Id
fv EUnit                      = S.empty
fv (EVar (TA _ x))            = S.singleton x
fv (EFun x _ (TA _ e))        = S.delete x $ fv e
fv (EApp (TA _ e1) (TA _ e2)) = S.union (fv e1) (fv e2)
fv (EFold _ (TA _ e))         = fv e
fv (EUnfold _ (TA _ e))       = fv e

type Fresh = ST.State Integer

type FreshId = Fresh Id

freshId :: FreshId
freshId = do
  n <- ST.get
  ST.put $ n + 1
  return $ "x" ++ show n

type FreshTExpr = Fresh TExpr

-- capture-avoiding substitution
-- sub x es et substitues es for x in et
-- sub x es et = et{es/x}
sub :: Id -> TExpr -> TExpr -> FreshTExpr
sub x es et = sub' et
  where
    sub' :: TExpr -> FreshTExpr
    sub' EUnit = return EUnit
    sub' var@(EVar (TA _ y))
      | y == x = return es
      | otherwise = return var
    sub' fn@(EFun y t (TA t' e))
      | y == x = return fn
      | otherwise = do
        if not $ S.member y $ fv es
          then do
            e' <- sub' e
            return $ EFun y t $ TA t' e'
          else do
            z <- freshId
            e' <- sub y (EVar (TA t z)) e
            e'' <- sub' e'
            return $ EFun z t $ TA t' e''
    sub' (EApp (TA t1 e1) (TA t2 e2)) = do
      e1' <- sub' e1
      e2' <- sub' e2
      return $ EApp (TA t1 e1') (TA t2 e2')
    sub' (EFold r (TA t e)) = do
      e' <- sub' e
      return $ EFold r $ TA t e'
    sub' (EUnfold r (TA t e)) = do
      e' <- sub' e
      return $ EUnfold r $ TA t e'
