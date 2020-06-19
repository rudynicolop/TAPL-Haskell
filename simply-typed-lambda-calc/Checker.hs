module Checker(check) where

import           AST
-- import qualified Control.Monad.Except as ERR
import qualified Data.Map.Strict as M

type Gamma = M.Map Id Type

data R = R Type TExpr

type Result = Maybe R

check :: Gamma -> BExpr -> Result
check g (ENat n)  = do return $ R TNat $ ENat n
check g (EBul b) = do return $ R TBul $ EBul b
check g (EVar (B x)) = do
  t <- M.lookup x g
  return $ R t $ EVar $ Ty t x
check g (ENot (B e)) = do
  e' <- check g e
  case e' of
    R TBul e'' -> do return $ R TBul $ ENot $ Ty TBul e''
    _          -> Nothing
check g (EAdd (B e1) (B e2)) = checkbi g TNat TNat EAdd e1 e2
check g (EMul (B e1) (B e2)) = checkbi g TNat TNat EMul e1 e2
check g (ESub (B e1) (B e2)) = checkbi g TNat TNat ESub e1 e2
check g (EEq  (B e1) (B e2)) = checkbi g TNat TBul EEq e1 e2
check g (ELeq (B e1) (B e2)) = checkbi g TNat TBul ELeq e1 e2
check g (EOr  (B e1) (B e2)) = checkbi g TBul TBul EOr e1 e2
check g (EAnd (B e1) (B e2)) = checkbi g TBul TBul EAnd e1 e2
check g (ECond (B e1) (B e2) (B e3)) = do
  e1' <- check g e1
  e2' <- check g e2
  e3' <- check g e3
  checkcond e1' e2' e3'
  where
    checkcond :: R -> R -> R -> Result
    checkcond (R TBul e1'') (R t2 e2'') (R t3 e3'')
      | t2 == t3 = return $ R t2 $ ECond (Ty TBul e1'') (Ty t2 e2'') (Ty t3 e3'')
      | otherwise = Nothing
    checkcond _ _ _ = Nothing
check g (ELam x t (B e)) = do
  R t' e' <- check (M.insert x t g) e
  return $ R (TArrow t t') (ELam x t $ Ty t' e')
check g (EApp (B e1) (B e2)) = do
  e1' <- check g e1
  e2' <- check g e2
  checkapp e1' e2'
  where
    checkapp :: R -> R -> Result
    checkapp (R (TArrow t1 t3) e1') (R t2 e2')
      | t1 == t3 = return $ R t3 $ EApp (Ty (TArrow t1 t3) e1') (Ty t1 e2')
      | otherwise = Nothing
    checkapp _ _ = Nothing

checkbi :: Gamma -> Type -> Type -> (T TExpr -> T TExpr -> TExpr) -> BExpr -> BExpr -> Result
checkbi g' t rt c e1 e2 = do
  e1' <- check g' e1
  e2' <- check g' e2
  checkbi' e1' e2'
    where
      checkbi' :: R -> R -> Result
      checkbi' (R t1 e1'') (R t2 e2'')
        | (t == t1 && t == t2) = return $ R rt $ c (Ty t e1'') (Ty t e2'')
        | otherwise = Nothing
