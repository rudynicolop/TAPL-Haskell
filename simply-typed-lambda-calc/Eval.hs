module Eval(eval,stepstar) where

import           AST
import qualified Checker                        as C
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
fv (EAdd (Ty _ e1) (Ty _ e2))  = S.union (fv e1) (fv e2)
fv (EMul (Ty _ e1) (Ty _ e2))  = S.union (fv e1) (fv e2)
fv (ESub (Ty _ e1) (Ty _ e2))  = S.union (fv e1) (fv e2)
fv (EEq (Ty _ e1) (Ty _ e2))   = S.union (fv e1) (fv e2)
fv (ELe (Ty _ e1) (Ty _ e2))   = S.union (fv e1) (fv e2)
fv (EAnd (Ty _ e1) (Ty _ e2))  = S.union (fv e1) (fv e2)
fv (EOr (Ty _ e1) (Ty _ e2))   = S.union (fv e1) (fv e2)
fv (ECond (Ty _ e1) (Ty _ e2) (Ty _ e3)) = S.union (fv e1) $ S.union (fv e2) (fv e3)
fv (ELam x _ (Ty _ e)) = S.delete x $ fv e
fv (EApp (Ty _ e1) (Ty _ e2)) = S.union (fv e1) (fv e2)

type Fresh = ST.State Integer

type FreshId = Fresh Id

freshId :: FreshId
freshId = do
  n <- ST.get
  ST.put $ n + 1
  return $ n |> show |> (++) "x"

type FreshTExpr = Fresh TExpr

subbi :: Id -> TExpr -> Ty TExpr -> Ty TExpr -> (Ty TExpr -> Ty TExpr -> TExpr) -> FreshTExpr
subbi x s (Ty t1 e1) (Ty t2 e2) bin = do
  e1' <- sub x s e1
  e2' <- sub x s e2
  return $ bin (Ty t1 e1') (Ty t2 e2')

-- TODO: capture-avoiding substitution
sub :: Id -> TExpr -> TExpr -> FreshTExpr
sub _ _ (ENat n) = do return $ ENat n
sub _ _ (EBul b) = do return $ EBul b
sub x s (EVar (Ty t y))
  | y == x = return s
  | y /= x = return $ EVar (Ty t y)
sub x s (EAdd e1 e2) = subbi x s e1 e2 EAdd
sub x s (EMul e1 e2) = subbi x s e1 e2 ESub
sub x s (ESub e1 e2) = subbi x s e1 e2 ESub
sub x s (EEq  e1 e2) = subbi x s e1 e2 EEq
sub x s (ELe  e1 e2) = subbi x s e1 e2 ELe
sub x s (EAnd e1 e2) = subbi x s e1 e2 EAnd
sub x s (EOr  e1 e2) = subbi x s e1 e2 EOr
sub x s (EApp e1 e2) = subbi x s e1 e2 EApp
sub x s (ECond (Ty t1 e1) (Ty t2 e2) (Ty t3 e3)) = do
  e1' <- sub x s e1
  e2' <- sub x s e2
  e3' <- sub x s e3
  return $ ECond (Ty t1 e1') (Ty t2 e2') (Ty t3 e3')
sub x s (ELam y t (Ty t' body))
  | y == x = do return $ ELam y t (Ty t' body)
  | y /= x && not isMem = do
    body' <- sub x s body
    return $ ELam y t (Ty t' body')
  | y /= x && isMem = do
    z <- freshId
    body' <- sub y (EVar (Ty t z)) body
    body'' <- sub x s body'
    return $ ELam z t (Ty t' body'')
  where
  isMem :: Bool
  isMem = s |> fv |> S.member y

bnot :: Bul -> Bul
bnot T = F
bnot F = T

nadd :: Nat -> Nat -> Nat
nadd Z n       = n
nadd (S n1) n2 = S $ nadd n1 n2

nmul :: Nat -> Nat -> Nat
nmul Z _       = Z
nmul (S n1) n2 = nadd n1 (nmul n1 n2)

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

-- why is the default lift NoProgress
instance AltMonadLift FIX.FixT where
 altLift m
  = FIX.FixT
  $ do  v <- m
        return (v, FIX.RunAgain)

-- The transformers-fix library is on opposite day...
halt :: Monad m => a -> FIX.FixT m a
halt a
 = FIX.FixT $ return (a, FIX.NoProgress)

-- normal order evaluation
step :: TExpr -> FreshFix
step (ENat n) = do return $ ENat n
step (EBul b) = do return $ EBul b
step (EVar x) = do return $ EVar x

-- algebraic operations
step (ENot (Ty TBul (EBul b))) = do
  return $ EBul $ bnot b
step (EAdd (Ty TNat (ENat n1)) (Ty TNat (ENat n2))) = do
  return $ ENat $ nadd n1 n2
step (EMul (Ty TNat (ENat n1)) (Ty TNat (ENat n2))) = do
  return $ ENat $ nmul n1 n2
step (ESub (Ty TNat (ENat n1)) (Ty TNat (ENat n2))) = do
  return $ ENat $ nsub n1 n2
step (EEq (Ty TNat (ENat n1)) (Ty TNat (ENat n2))) = do
  return $ EBul $ eeq n1 n2
step (ELe (Ty TNat (ENat n1)) (Ty TNat (ENat n2))) = do
  return $ EBul $ ele n1 n2
step (EAnd (Ty TBul (EBul b1)) (Ty TBul (EBul b2))) = do
  return $ EBul $ band b1 b2
step (EOr (Ty TBul (EBul b1)) (Ty TBul (EBul b2))) = do
  return $ EBul $ bor b1 b2

-- algebraic reductions
step (ENot (Ty TBul e)) = do
  e' <- step e
  return $ ENot $ Ty TBul e'

-- left-hand algebraic reductions
step (EAdd (Ty TNat e1) e2) = do
  e1' <- step e1
  return $ EAdd (Ty TNat e1') e2
step (EMul (Ty TNat e1) e2) = do
  e1' <- step e1
  return $ EMul (Ty TNat e1') e2
step (ESub (Ty TNat e1) e2) = do
  e1' <- step e1
  return $ ESub (Ty TNat e1') e2
step (EEq (Ty TNat e1) e2) = do
  e1' <- step e1
  return $ EEq (Ty TNat e1') e2
step (ELe (Ty TNat e1) e2) = do
  e1' <- step e1
  return $ ELe (Ty TNat e1') e2
step (EAnd (Ty TBul e1) e2) = do
  e1' <- step e1
  return $ EAnd (Ty TBul e1') e2
step (EOr (Ty TBul e1) e2) = do
  e1' <- step e1
  return $ EOr (Ty TBul e1') e2

-- right-hand algebraic reductions
step (EAdd (Ty TNat (ENat n1')) (Ty TNat e2)) = do
  e2' <- step e2
  return $ EAdd (Ty TNat (ENat n1')) (Ty TNat e2')
step (EMul (Ty TNat (ENat n1')) (Ty TNat ee2)) = do
  e2' <- step ee2
  return $ EMul (Ty TNat (ENat n1')) (Ty TNat e2')
step (ESub (Ty TNat (ENat n1')) (Ty TNat ee2)) = do
  e2' <- step ee2
  return $ ESub (Ty TNat (ENat n1')) (Ty TNat e2')
step (EEq (Ty TNat (ENat n1')) (Ty TNat ee2)) = do
  e2' <- step ee2
  return $ EEq (Ty TNat (ENat n1')) (Ty TNat e2')
step (ELe (Ty TNat (ENat n1')) (Ty TNat ee2)) = do
  e2' <- step ee2
  return $ ELe (Ty TNat (ENat n1')) (Ty TNat e2')
step (EAnd (Ty TBul (EBul b1')) (Ty TBul ee2)) = do
  e2' <- step ee2
  return $ EAnd (Ty TBul (EBul b1')) (Ty TBul e2')
step (EOr (Ty TBul (EBul b1')) (Ty TBul ee2)) = do
  e2' <- step ee2
  return $ EOr (Ty TBul (EBul b1')) (Ty TBul e2')

-- control flow reductions
step (ECond (Ty TBul (EBul T)) (Ty _ e1) _) = do return e1
step (ECond (Ty TBul (EBul F)) _ (Ty _ e2)) = do return e2
step (ECond (Ty TBul e) (Ty t1 e1) (Ty t2 e2)) = do
  e' <- step e
  return $ ECond (Ty TBul e') (Ty t1 e1) (Ty t2 e2)
step (ELam x t (Ty t' e)) = do
  e' <- step e
  return $ ELam x t (Ty t' e')
step (EApp (Ty _ (ELam x _ (Ty _ e1))) (Ty _ e2)) = altLift $ sub x e2 e1
step (EApp (Ty t1 e1) (Ty t2 e2)) = do
  e1' <- step e1
  if e1' == e1 then do
    e2' <- step e2
    return $ EApp (Ty t1 e1) (Ty t2 e2')
  else do return $ EApp (Ty t1 e1') (Ty t2 e2)

-- default case, stuck or terminated
step e = halt e

-- steps until stuck/fixpoint
stepstar :: TExpr -> TExpr
stepstar e = ST.evalState (FIX.fixpoint step e) 0

-- TODO
eval :: TExpr -> Maybe Value
eval _ = Nothing
