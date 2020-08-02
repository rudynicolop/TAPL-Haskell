{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Check where

import           AST
import qualified Control.Monad.Except as ERR
import qualified Data.Map.Strict      as M

type Gamma = M.Map Id Type

data R = R Type TExpr

type Result = Either String R

check :: (Annotation t, Show (t (Pattern t)), Show (t (Expr t))) =>
  Gamma -> Expr t -> Result
check g (EName ax) =
  let x = gx ax in do
    case M.lookup x g of
      Nothing -> ERR.throwError $ "Unbound variable " ++ x
      Just t  -> return $ R t $ EName $ T t x
check g EUnit = return $ R TUnit $ EUnit
check g app@(EApp e1 e2) = do
  e1' <- check g $ ge e1
  e2' <- check g $ ge e2
  checkapp e1' e2'
  where
    checkapp :: R -> R -> Result
    checkapp (R (TFun t1 t3) e1') (R t2 e2')
      | t1 == t2 = return $ R t3 $ EApp (T (TFun t1 t3) e1') (T t1 e2')
      | otherwise = ERR.throwError $
        "In application " ++ (show app) ++ ", argument " ++ (show e2') ++
          " is expected to have type " ++ (show t1) ++
            ", but has type " ++ (show t2)
    checkapp (R t1 _) _ = ERR.throwError $
      "In application " ++ (show app) ++
        ", expression " ++ (show e1) ++ " has type " ++ (show t1)
check g (EPair e1 e2) = do
  R a e1' <- check g $ ge e1
  R b e2' <- check g $ ge e2
  return $ R (TPair a b) (EPair (T a e1') (T b e2'))
check g efst@(EFst e) = do
  e' <- check g $ ge e
  checkFst e'
  where
    checkFst :: R -> Result
    checkFst (R (TPair a b) e') = return $ R a (EFst (T (TPair a b) e'))
    checkFst (R t' e') = ERR.throwError $
      "In expression " ++ (show efst) ++ ", sub-expression " ++ (show e') ++
        " is expected to have a product type, but has type " ++ (show t')
check g esnd@(ESnd e) = do
  e' <- check g $ ge e
  checkSnd e'
  where
    checkSnd :: R -> Result
    checkSnd (R (TPair a b) e') = return $ R b (EFst (T (TPair a b) e'))
    checkSnd (R t' e') = ERR.throwError $
      "In expression " ++ (show esnd) ++ ", sub-expression " ++ (show e') ++
        " is expected to have a product type, but has type " ++ (show t')
check g left@(ELeft a b e) = do
  R a' e' <- check g $ ge e
  if a' == a
    then return $ R (TEither a b) (ELeft a b (T a e'))
    else ERR.throwError $ "In expression " ++ (show left) ++
      ", sub-expression " ++ (show e') ++ " is expected to have type " ++
      (show a) ++ ", but has type " ++ (show a')
check g right@(ERight a b e) = do
  R b' e' <- check g $ ge e
  if b' == b
    then return $ R (TEither a b) (ELeft a b (T b e'))
    else ERR.throwError $ "In expression " ++ (show right) ++
      ", sub-expression " ++ (show e') ++ " is expected to have type " ++
      (show b) ++ ", but has type " ++ (show b')
check _ _ = ERR.throwError "need to implement more cases"
