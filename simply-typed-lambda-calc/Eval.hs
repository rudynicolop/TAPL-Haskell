{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Eval(stepstar) where

import           AST
import qualified Control.Monad.IO.Class         as MIO
import qualified Control.Monad.Trans.Class      as MT
import qualified Control.Monad.Trans.Fix        as FIX
import qualified Control.Monad.Trans.State.Lazy as ST
import qualified Data.Map.Strict                as M
import qualified Data.Set                       as S

-- set of free variables in an expression
fv :: TExpr -> S.Set String
fv (ENat _)                    = S.empty
fv (EBul _)                    = S.empty
fv (EVar (Ty _ x))             = S.singleton x
fv (ENot (Ty _ e))             = fv e
fv (EBOp _ (Ty _ e1) (Ty _ e2)) = S.union (fv e1) (fv e2)
fv (ECond (Ty _ e1) (Ty _ e2) (Ty _ e3)) = S.union (fv e1) $ S.union (fv e2) (fv e3)
fv (ELam x _ (Ty _ e)) = S.delete x $ fv e
fv (EApp (Ty _ e1) (Ty _ e2)) = S.union (fv e1) (fv e2)

type Fresh = ST.StateT Integer IO

type FreshId = Fresh Id

freshId :: FreshId
freshId = do
  n <- ST.get
  ST.put $ n + 1
  return $ n |> show |> (++) "x"

type FreshTExpr = Fresh TExpr

-- capture-avoiding substitution
sub :: Id -> TExpr -> TExpr -> FreshTExpr
sub _ _ (ENat n) = do return $ ENat n
sub _ _ (EBul b) = do return $ EBul b
sub x s (EVar (Ty t y))
  | y == x = return s
  | otherwise = return $ EVar (Ty t y)
sub x s (ENot (Ty t e)) = do
  e' <- sub x s e
  return $ ENot $ Ty t e'
sub x s (EBOp bop (Ty t1 e1) (Ty t2 e2)) = do
  e1' <- sub x s e1
  e2' <- sub x s e2
  return $ EBOp bop (Ty t1 e1') (Ty t2 e2')
sub x s (EApp (Ty t1 e1) (Ty t2 e2)) = do
  e1' <- sub x s e1
  e2' <- sub x s e2
  return $ EApp (Ty t1 e1') (Ty t2 e2')
sub x s (ECond (Ty t1 e1) (Ty t2 e2) (Ty t3 e3)) = do
  e1' <- sub x s e1
  e2' <- sub x s e2
  e3' <- sub x s e3
  return $ ECond (Ty t1 e1') (Ty t2 e2') (Ty t3 e3')
sub x s (ELam y t (Ty t' body))
  | y == x = do return $ ELam y t (Ty t' body)
  | otherwise = do
    if not $ S.member y $ fv s
      then do
        body' <- sub x s body
        return $ ELam y t (Ty t' body')
      else do
        z <- freshId
        body' <- sub y (EVar (Ty t z)) body
        body'' <- sub x s body'
        return $ ELam z t (Ty t' body'')

bnot :: Bul -> Bul
bnot T = F
bnot F = T

nadd :: Nat -> Nat -> Nat
nadd Z n       = n
nadd (S n1) n2 = S $ nadd n1 n2

nmul :: Nat -> Nat -> Nat
nmul Z _       = Z
nmul (S n1) n2 = nadd n2 (nmul n1 n2)

nsub :: Nat -> Nat -> Nat
nsub Z _           = Z
nsub n Z           = n
nsub (S n1) (S n2) = nsub n1 n2

ele :: Nat -> Nat -> Bul
ele n1 n2 =
  case nsub n2 n1 of
    S _ -> T
    Z   -> F

eeq :: Nat -> Nat -> Bul
eeq n1 n2 =
  case (nsub n1 n2, nsub n2 n1) of
    (Z,Z) -> T
    _     -> F

band :: Bul -> Bul -> Bul
band T T = T
band F _ = F
band _ F = F

bor :: Bul -> Bul -> Bul
bor F F = F
bor T _ = T
bor _ T = T

type FreshFix = FIX.FixT Fresh TExpr

class (MT.MonadTrans mt) => AltMonadLift mt where
  altLift :: Monad m => m a -> mt m a

-- why is the default lift NoProgress?
instance AltMonadLift FIX.FixT where
 altLift m
  = FIX.FixT
  $ do  v <- m
        return (v, FIX.RunAgain)

-- The transformers-fix library is on opposite day...
halt :: Monad m => a -> FIX.FixT m a
halt a
 = FIX.FixT $ return (a, FIX.NoProgress)

instance MIO.MonadIO (FIX.FixT Fresh) where
 liftIO a = FIX.FixT
   $ do
     v <- MIO.liftIO a
     return (v, FIX.NoProgress)

printFreshFix :: TExpr -> FreshFix
printFreshFix e = do
  MIO.liftIO $ putStrLn " -> "
  MIO.liftIO $ putStrLn $ show e
  return e

-- lazy evaluation
stepWrap :: TExpr -> FreshFix
stepWrap expr = do
  expr' <- printFreshFix expr
  step expr'
  where
    step :: TExpr -> FreshFix

    -- algebraic operations
    step (ENot (Ty TBul (EBul b))) = do
      FIX.progress $ EBul $ bnot b
    step bop@(EBOp op (Ty TNat (ENat n1)) (Ty TNat (ENat n2))) = do
      case op of
        EAdd -> FIX.progress $ ENat $ nadd n1 n2
        EMul -> FIX.progress $ ENat $ nmul n1 n2
        ESub -> FIX.progress $ ENat $ nsub n1 n2
        EEq  -> FIX.progress $ EBul $ eeq  n1 n2
        ELe  -> FIX.progress $ EBul $ ele  n1 n2
        _    -> halt bop
    step bop@(EBOp op (Ty TBul (EBul b1)) (Ty TBul (EBul b2))) = do
      case op of
        EAnd -> FIX.progress $ EBul $ band b1 b2
        EOr  -> FIX.progress $ EBul $ bor  b1 b2
        _    -> halt bop

    -- algebraic reductions
    step (ENot (Ty TBul e)) = do
      e' <- step e
      FIX.progress $ ENot $ Ty TBul e'

    -- right-hand algebraic reductions
    step (EBOp bop (Ty TNat (ENat n1)) (Ty t2 e2)) = do
      e2' <- step e2
      FIX.progress $ EBOp bop (Ty TNat (ENat n1)) (Ty t2 e2')
    step (EBOp bop (Ty TBul (EBul b1)) (Ty t2 e2)) = do
      e2' <- step e2
      FIX.progress $ EBOp bop (Ty TBul (EBul b1)) (Ty t2 e2')

    -- left-hand algebraic reductions
    step (EBOp bop (Ty t1 e1) e2) = do
      e1' <- step e1
      FIX.progress $ EBOp bop (Ty t1 e1') e2

    -- control flow reductions
    step (ECond (Ty TBul (EBul T)) (Ty _ e1) _) = FIX.progress e1
    step (ECond (Ty TBul (EBul F)) _ (Ty _ e2)) = FIX.progress e2
    step (ECond (Ty TBul e) (Ty t1 e1) (Ty t2 e2)) = do
      e' <- step e
      FIX.progress $ ECond (Ty TBul e') (Ty t1 e1) (Ty t2 e2)
    step (EApp (Ty _ (ELam x _ (Ty _ e1))) (Ty _ e2)) = altLift $ sub x e2 e1
    step (EApp (Ty t1 e1) (Ty t2 e2)) = do
      e1' <- step e1
      FIX.progress $ EApp (Ty t1 e1') (Ty t2 e2)

    -- default case, stuck or terminated
    step e = halt e

-- steps until stuck/fixpoint
stepstar :: TExpr -> IO TExpr
stepstar e = ST.evalStateT (FIX.fixpoint stepWrap e) 0
