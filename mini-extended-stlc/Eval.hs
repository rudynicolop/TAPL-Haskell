{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Eval where

import           AST
import qualified Check                as C
import qualified Control.Monad.Except as ERR
import qualified Data.Map.Strict      as M
import qualified Data.Set             as S

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

-- binds sub-values to variables in a pattern if it is an instance
instanceBind :: TPattern -> TValue -> TBinds -> Maybe TBinds
instanceBind (PBase (T _ Nothing)) _ b = Just b
instanceBind (PBase (T t (Just x))) v b = Just $ M.insert x (T t v) b
instanceBind PUnit VUnit b = Just b
instanceBind (PPair (T _ p1) (T _ p2)) (VPair (T _ v1) (T _ v2)) b = do
  b1 <- instanceBind p1 v1 b
  b2 <- instanceBind p2 v2 b
  Just $ M.union b1 b2
instanceBind (PLeft a b (T _ p)) (VLeft a' b' (T _ v)) bds
  | a == a' && b == b' = instanceBind p v bds
  | otherwise = Nothing
instanceBind (PRight a b (T _ p)) (VRight a' b' (T _ v)) bds
  | a == a' && b == b' = instanceBind p v bds
  | otherwise = Nothing
instanceBind (POr (T _ p1) (T _ p2)) v b =
  case instanceBind p1 v b of
    Nothing -> instanceBind p2 v b
    Just b' -> Just b'
instanceBind _ _ _ = Nothing

-- big-step semantics...too lazy to do substitution with patterns...
eval :: TBinds -> TExpr -> Either String TValue
eval b (EName (T _ x)) =
  case M.lookup x b of
    Nothing      -> ERR.throwError $ "Unbound variable " ++ x
    Just (T _ v) -> return v
eval _ EUnit = return $ VUnit
eval b (EFun (T tp p) t (T te e)) = return $ VClosure b (T tp p) t (T te e)
eval b expr@(EApp (T t1 e1) (T t2 e2)) = do
  v1 <- eval b e1
  v2 <- eval b e2
  case v1 of
    closure@(VClosure b' (T _ p) t (T _ e)) -> do
      case instanceBind p v2 b' of
        Nothing -> ERR.throwError $ "Pattern " ++ (show p) ++ " in closure "
          ++ (show closure) ++ " failed to bind with value " ++ (show v2) ++
            " in application " ++ (show expr)
        Just b'' -> eval b'' e
    _ -> ERR.throwError $ "In application " ++ (show expr) ++
      ", expression" ++ (show e1) ++
        " is expected to evaluate to a closure, but evaluated to " ++ (show v1)
eval b expr@(ELet (T _ p) (T _ e1) (T _ e2)) = do
  v1 <- eval b e1
  case instanceBind p v1 b of
    Nothing -> ERR.throwError $ "Pattern " ++ (show p) ++ " in let-expression "
      ++ (show expr) ++ " failed to bind with value " ++ (show v1)
    Just b' -> eval b' e2
eval b (EPair (T t1 e1) (T t2 e2)) = do
  v1 <- eval b e1
  v2 <- eval b e2
  return $ VPair (T t1 v1) (T t2 v2)
eval b expr@(EFst (T _ e)) = do
  v <- eval b e
  case v of
    (VPair (T _ v1) _) -> return v1
    _ -> ERR.throwError $ "In expression " ++ (show expr) ++ ", sub-expression " ++
      (show e) ++ " is expected to evaluate to a pair, but evaluated to " ++ (show v)
eval b expr@(ESnd (T _ e)) = do
  v <- eval b e
  case v of
    (VPair _ (T _ v2)) -> return v2
    _ -> ERR.throwError $ "In expression " ++ (show expr) ++ ", sub-expression " ++
      (show e) ++ " is expected to evaluate to a pair, but evaluated to " ++ (show v)
eval bds (ELeft a b (T t e)) = do
  v <- eval bds e
  return $ VLeft a b (T t v)
eval bds (ERight a b (T t e)) = do
  v <- eval bds e
  return $ VRight a b (T t v)
eval bds match@(EMatch (T t e) cases) = do
  v <- eval bds e
  rowFilters v cases
  where
    rowFilters :: TValue -> [(T TPattern, T TExpr)] -> Either String TValue
    rowFilters v [] = ERR.throwError $ "Expression " ++ (show e) ++
      " in match-expression " ++ (show match) ++ " evaluated to " ++ (show v) ++
        " and failed to bind to any pattern"
    rowFilters v ((T _ hp, T _ he):tcases) =
      case instanceBind hp v bds of
        Nothing   -> rowFilters v tcases
        Just bds' -> eval bds' he
