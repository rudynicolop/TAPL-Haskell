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

type FreshName = Fresh String

-- fresh ~:: Integer -> (String, Integer)
freshName :: FreshName
-- fresh = ST.state $ \ n -> ("x"++(show n),n+1)
freshName = do
  n <- ST.get
  ST.put $ n + 1
  return $ n |> show |> (++) "x"

type FreshExpr = Fresh Expr

-- TODO: capture avoiding substitution
subf :: FreshExpr -> FreshExpr -> String -> FreshExpr
subf fe fs x = do
  e <- fe
  s <- fs
  sub e s x
  where
  sub :: Expr -> Expr -> String -> FreshExpr
  sub (Var y) s x
    | y == x = return s
    | y /= x = return $ Var y
  sub (App e1 e2) s x = do
    e1' <- sub e1 s x
    e2' <- sub e2 s x
    return $ App e1' e2'
  sub (Lam y body) s x
    | y == x = return $ Lam y body
    | y /= x && not isMem = do
      body' <- sub body s x
      return $ Lam y body'
    | y /= x && isMem = do
      z <- freshName
      body' <- sub body (Var z) y
      body'' <- sub body' s x
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

-- normal order reduction
step :: Expr -> Maybe Expr
step (Var x) = Nothing
step (Lam x e) = do
  e' <- step e
  return $ Lam x e'
step (App (Lam x e1) e2) = Nothing -- TODO: proper substitution return $ sub e1 e2 x
step (App e1 e2) =
  case step e1 of
    Nothing -> do
      e2' <- step e2
      return $ App e1 e2'
    Just e1' -> return $ App e1' e2
