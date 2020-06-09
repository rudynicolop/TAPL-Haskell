module Eval where

import           AST
import qualified Control.Monad.Trans.State.Lazy as ST
import qualified Data.Set                       as S

-- set of free variables in an expression
fv :: Expr -> S.Set String
fv (Var x)     = S.singleton x
fv (Lam x e)   = e |> fv |> S.delete x
fv (App e1 e2) = S.union (fv e1) (fv e2)

type Fresh a = ST.State Integer a

-- ~ Integer -> (String,Integer)
type FreshName = Fresh String

-- fresh ~:: Integer -> (String, Integer)
freshName :: FreshName
-- fresh = ST.state $ \ n -> ("x"++(show n),n+1)
freshName = do
  n <- ST.get
  ST.put $ n + 1
  return $ n |> show |> (++) "x"

-- ~ Integer -> (Expr,Integer)
type FreshExpr = Fresh Expr

-- TODO: capture avoiding substitution
-- [sub x s e n] substitues s for x in e with state n
sub :: String -> Expr -> Expr -> FreshExpr
sub x s (Var y)
  | y == x = return s
  | y /= x = return $ Var y
sub x s (App e1 e2) = do
  e1' <- sub x s e1
  e2' <- sub x s e2
  return $ App e1' e2'
sub x s (Lam y body)
  | y == x = return $ Lam y body
  | y /= x && not isMem = do
    body' <- sub x s body
    return $ Lam y body'
  | y /= x && isMem = do
    z <- freshName
    body' <- sub y (Var z) body
    body'' <- sub x s body'
    return $ Lam z body''
  where
  isMem :: Bool
  isMem = s |> fv |> S.member y

{-
StateT s m a ~= s -> m (a,s)

State s a = StateT s Identity a ~= s -> (a,s)

FreshT = StateT FreshState m a = StateT Integer m a ~= Integer -> m (a,Integer)

Fresh = FreshT Identity a = StateT FreshState Identity a = State FreshState a = State Integer a ~= Integer -> (a,Integer)

-}

-- ~ Integer -> Maybe (Expr,Integer)
type FreshExprT = ST.StateT Integer Maybe Expr

-- normal order reduction
-- ~Expr -> Integer -> Maybe (Expr,Integer)
step :: Expr -> FreshExprT
step (Var _) = ST.StateT $ \ _ -> Nothing
step (Lam x e) = do
  e' <- step e
  return $ Lam x e'
step (App (Lam x e1) e2) = ST.StateT $ \n -> Just $ ST.runState (sub x e2 e1) n
step (App e1 e2) =
  ST.StateT $ \ n ->
    case ST.runStateT (step e1) n of
      Nothing ->
        case ST.runStateT (step e2) n of
          Nothing       -> Nothing
          Just (e2',n') -> Just (App e1 e2',n')
      Just (e1',n') -> Just (App e1' e2,n')
