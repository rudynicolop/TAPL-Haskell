module Checker(check,R(..)) where

import           AST
-- import qualified Control.Monad.Except as ERR
import qualified Data.Map.Strict as M

type Gamma = M.Map Id Type

data R = R Type TExpr

type Result = Maybe R

check :: Annotation t => Gamma -> Expr t -> Result
check g (ENat n)  = do return $ R TNat $ ENat n
check g (EBul b) = do return $ R TBul $ EBul b
check g (EVar tx) = do
  let x = gx tx in do
    t <- M.lookup x g
    return $ R t $ EVar $ Ty t x
check g (ENot e) = do
  e' <- e |> ge |> check g
  case e' of
    R TBul e'' -> do return $ R TBul $ ENot $ Ty TBul e''
    _          -> Nothing
check g (EAdd e1 e2) = checkbi g TNat TNat EAdd e1 e2
check g (EMul e1 e2) = checkbi g TNat TNat EMul e1 e2
check g (ESub e1 e2) = checkbi g TNat TNat ESub e1 e2
check g (EEq  e1 e2) = checkbi g TNat TBul EEq e1 e2
check g (ELe  e1 e2) = checkbi g TNat TBul ELe e1 e2
check g (EOr  e1 e2) = checkbi g TBul TBul EOr e1 e2
check g (EAnd e1 e2) = checkbi g TBul TBul EAnd e1 e2
check g (ECond e1 e2 e3) = do
  e1' <- e1 |> ge |> check g
  e2' <- e2 |> ge |> check g
  e3' <- e3 |> ge |> check g
  checkcond e1' e2' e3'
  where
    checkcond :: R -> R -> R -> Result
    checkcond (R TBul e1'') (R t2 e2'') (R t3 e3'')
      | t2 == t3 = return $ R t2 $ ECond (Ty TBul e1'') (Ty t2 e2'') (Ty t3 e3'')
      | otherwise = Nothing
    checkcond _ _ _ = Nothing
check g (ELam x t e) = do
  R t' e' <- e |> ge |> check (M.insert x t g)
  return $ R (TArrow t t') (ELam x t $ Ty t' e')
check g (EApp e1 e2) = do
  e1' <- e1 |> ge |> check g
  e2' <- e2 |> ge |> check g
  checkapp e1' e2'
  where
    checkapp :: R -> R -> Result
    checkapp (R (TArrow t1 t3) e1') (R t2 e2')
      | t1 == t2 = return $ R t3 $ EApp (Ty (TArrow t1 t3) e1') (Ty t1 e2')
      | otherwise = Nothing
    checkapp _ _ = Nothing

checkbi :: Annotation t => Gamma -> Type -> Type -> (Ty TExpr -> Ty TExpr -> TExpr) -> t (Expr t) -> t (Expr t) -> Result
checkbi g t rt c e1 e2 = do
  e1' <- e1 |> ge |> check g
  e2' <- e2 |> ge |> check g
  checkbi' e1' e2'
    where
      checkbi' :: R -> R -> Result
      checkbi' (R t1 e1'') (R t2 e2'')
        | (t == t1 && t == t2) = return $ R rt $ c (Ty t e1'') (Ty t e2'')
        | otherwise = Nothing
