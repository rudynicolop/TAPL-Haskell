module Eval(eval) where

import           AST
import qualified Control.Monad.Trans.Class      as MT
import qualified Control.Monad.Trans.State.Lazy as ST
import qualified Data.Set                       as S

(|>) x f = f x

-- TODO: set of free variables in an expression
fv :: TExpr -> S.Set String
fv _ = S.empty

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
