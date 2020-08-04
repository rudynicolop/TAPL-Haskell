{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Eval where

import           AST
import qualified Check                          as C
import qualified Control.Monad.Trans.Class      as MT
import qualified Control.Monad.Trans.Fix        as FIX
import qualified Control.Monad.Trans.State.Lazy as ST
import qualified Data.Map.Strict                as M
import qualified Data.Set                       as S

-- set of free variables in an expression
fv :: TExpr -> S.Set Id
fv (EName (T _ x)) = S.singleton x
fv EUnit           = S.empty
fv (EFun (T _ p) _ (T _ e)) = S.difference (fv e) (C.fv p)
fv (EApp (T _ e1) (T _ e2)) = S.union (fv e1) (fv e2)
fv (ELet (T _ p) (T _ e1) (T _ e2)) =
  S.union (fv e1) $ S.difference (fv e2) (C.fv p)
fv (EPair (T _ e1) (T _ e2))    = S.union (fv e1) (fv e2)
fv (EFst (T _ e))         = fv e
fv (ESnd (T _ e))         = fv e
fv (ELeft _ _ (T _ e))    = fv e
fv (ERight _ _ (T _ e))   = fv e
fv (EMatch (T _ e) cases) = foldl fvCase (fv e) cases
  where
    fvCase :: S.Set Id -> (T TPattern, T TExpr) -> S.Set Id
    fvCase acc (T _ p', T _ e') = S.union acc $ S.difference (fv e') (C.fv p')

type Fresh = ST.State Int

type FreshId = Fresh Id

freshId :: FreshId
freshId = do
  n <- ST.get
  ST.put $ n + 1
  return $  (++) "x" $ show n

type FreshTExpr = Fresh TExpr

type Bindings = M.Map Id TValue

-- binds sub-values to variables in a pattern if it is an instance
instanceBind :: TPattern -> TValue -> Maybe Bindings
instanceBind (PBase (T _ Nothing)) _  = Just M.empty
instanceBind (PBase (T _ (Just x))) v = Just $ M.singleton x v
instanceBind PUnit VUnit              = Just M.empty
instanceBind (PPair (T _ p1) (T _ p2)) (VPair (T _ v1) (T _ v2)) = do
  b1 <- instanceBind p1 v1
  b2 <- instanceBind p2 v2
  Just $ M.union b1 b2
instanceBind (PLeft a b (T _ p)) (VLeft a' b' (T _ v))
  | a == a' && b == b' = instanceBind p v
  | otherwise = Nothing
instanceBind (PRight a b (T _ p)) (VRight a' b' (T _ v))
  | a == a' && b == b' = instanceBind p v
  | otherwise = Nothing
instanceBind (POr (T _ p1) (T _ p2)) v =
  case instanceBind p1 v of
    Nothing -> instanceBind p2 v
    Just b  -> Just b
instanceBind _ _ = Nothing

ofValue :: TValue -> TExpr
ofValue VUnit                      = EUnit
ofValue (VFun (T tp p) t (T te e)) = EFun (T tp p) t (T te e)
ofValue (VPair (T t1 v1) (T t2 v2)) = EPair (T t1 $ ofValue v1) (T t2 $ ofValue v2)
ofValue (VLeft a b (T tv v)) = ELeft a b (T tv $ ofValue v)
ofValue (VRight a b (T tv v)) = ERight a b (T tv $ ofValue v)

toValue :: TExpr -> Maybe TValue
toValue EUnit                      = Just VUnit
toValue (EFun (T tp p) t (T te e)) = Just $ VFun (T tp p) t (T te e)
toValue (EPair (T t1 e1) (T t2 e2)) = do
  v1 <- toValue e1
  v2 <- toValue e2
  Just $ VPair (T t1 v1) (T t2 v2)
toValue (ELeft a b (T te e)) = do
  v <- toValue e
  Just $ VLeft a b (T te v)
toValue (ERight a b (T te e)) = do
  v <- toValue e
  Just $ VRight a b (T te v)
toValue _ = Nothing
