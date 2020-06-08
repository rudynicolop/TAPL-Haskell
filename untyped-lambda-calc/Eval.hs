module Eval where

import           AST

-- TODO: capture avoiding substitution
sub :: Expr -> Expr -> String -> Expr
sub e _ _ = e

-- normal order reduction
step :: Expr -> Maybe Expr
step (Var x) = Nothing
step (Lam x e) = do
  e' <- step e
  return $ Lam x e'
step (App (Lam x e1) e2) = return $ sub e1 e2 x
step (App e1 e2) =
  case step e1 of
    Nothing -> do
      e2' <- step e2
      return $ App e1 e2'
    Just e1' -> return $ App e1' e2
