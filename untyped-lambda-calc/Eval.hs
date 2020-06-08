module Eval where

import           AST
import qualified Data.Set as S
import qualified Control.Fresh as F

-- set of free variables in an expression
fv :: Expr -> S.Set String
fv (Var x) = S.singleton x
fv (Lam x e) = e |> fv |> S.delete x
fv (App e1 e2) = S.union (fv e1) (fv e2)

-- TODO: capture avoiding substitution
sub :: Expr -> Expr -> String -> Expr
sub (Var y) s x
  | y == x = s
  | y /= x = y
sub (App e1 e2) s x = App (sub e1 s x) (sub e2 s x)
sub (Lam y e) s x
  | y == x = Lam y e
  | y /= x && S.notMember y (fv s)
  | _ -> s --TODO: need to use a fresh monad for name generation...seems tricky...

-- initially all used names are names in the given program
-- => initial state is set of names in given program
-- when performing substitution, needs to "generate" a fresh name
-- so state also includes the next available name,...maybe...
-- every transformation will return the next available name,
-- update the set of used names, and the next available name

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
