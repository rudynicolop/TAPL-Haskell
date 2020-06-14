module Eval(eval) where

import           AST
import qualified Control.Monad.Trans.Class      as MT
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
freshName = do
  n <- ST.get
  ST.put $ n + 1
  return $ n |> show |> (++) "x"

-- ~ Integer -> (Expr,Integer)
type FreshExpr = Fresh Expr

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

-- ~ Integer -> Maybe (Expr,Integer)
type FreshExprT = ST.StateT Integer Maybe Expr

hoistState :: Monad m => ST.State s a -> ST.StateT s m a
hoistState = ST.state . ST.runState

hoistFreshMaybe :: FreshExpr -> FreshExprT
hoistFreshMaybe = hoistState
-- normal order reduction
-- ~Expr -> Integer -> Maybe (Expr,Integer)
step :: Expr -> FreshExprT
step (Var _) = MT.lift Nothing
step (Lam x e) = do
  e' <- step e
  return $ Lam x e'
step (App (Lam x e1) e2) = hoistFreshMaybe (sub x e2 e1)
step (App e1 e2) =
  ST.StateT $ \ n ->
    case ST.runStateT (step e1) n of
      Nothing ->
        case ST.runStateT (step e2) n of
          Nothing       -> Nothing
          Just (e2',n') -> Just (App e1 e2',n')
      Just (e1',n') -> Just (App e1' e2,n')

-- evaluates a lambda Calculus program to its normal form
eval :: Expr -> IO Expr
eval e =
  evalf 0 e
  where
  evalf :: Integer -> Expr -> IO Expr
  evalf n e = do
    putStrLn $ show $ e
    case ST.runStateT (step e) n of
      Nothing      -> return e -- normal form
      Just (e',n') -> evalf n' e'
