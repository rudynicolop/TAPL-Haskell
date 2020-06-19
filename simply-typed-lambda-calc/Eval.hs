module Eval(eval) where

import           AST
import qualified Control.Monad.Trans.Class      as MT
import qualified Control.Monad.Trans.State.Lazy as ST
import qualified Data.Set                       as S

(|>) x f = f x

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
fv (ELeq (Ty _ e1) (Ty _ e2))  = S.union (fv e1) (fv e2)
fv (EAnd (Ty _ e1) (Ty _ e2))  = S.union (fv e1) (fv e2)
fv (EOr (Ty _ e1) (Ty _ e2))   = S.union (fv e1) (fv e2)
fv (ECond (Ty _ e1) (Ty _ e2) (Ty _ e3)) = S.union (fv e1) $ S.union (fv e2) (fv e3)
fv (ELam x _ (Ty _ e)) = S.delete x $ fv e
fv (EApp (Ty _ e1) (Ty _ e2)) = S.union (fv e1) (fv e2)

type Fresh a = ST.State Integer a

type FreshId = Fresh Id

freshId :: FreshId
freshId = do
  n <- ST.get
  ST.put $ n + 1
  return $ n |> show |> (++) "x"

type FreshTExpr = Fresh TExpr

type FreshTExprT = ST.StateT Integer Maybe TExpr

hoistState :: Monad m => ST.State s a -> ST.StateT s m a
hoistState = ST.state . ST.runState

hoistFreshMaybe :: FreshTExpr -> FreshTExprT
hoistFreshMaybe = hoistState

step :: TExpr -> FreshTExprT
step (ENat n) = do return $ ENat n

eval :: TExpr -> Maybe Value
eval _ = Nothing
