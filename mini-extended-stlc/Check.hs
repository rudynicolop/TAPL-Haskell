{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Check where

import           AST
import qualified Control.Monad.Except as ERR
import qualified Data.Map.Strict      as M

type Gamma = M.Map Id Type

data R = R Type TExpr

type Result = Either String R

data RP = RP Gamma TPattern

type ResultP = Either String RP

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

checkPattern :: (Annotation t, Show (t (Pattern t))) =>
  Gamma -> Type -> Pattern t -> ResultP
checkPattern g t PWild     = return $ RP g PWild
checkPattern g t (PName x) = return $ RP (M.insert (gx x) t g) (PName (T t $ gx x))
checkPattern g TUnit PUnit = return $ RP g PUnit
checkPattern g (TPair a b) pat@(PPair p1 p2) = do
  RP g1 p1' <- checkPattern g a $ gp p1
  RP g2 p2' <- checkPattern g b $ gp p2
  if M.disjoint g1 g2
    then return $ RP (M.union g1 g2) (PPair (T a p1') (T b p2'))
    else ERR.throwError $ "In pattern " ++ (show pat) ++
      ", sub-patterns " ++ (show p1') ++ " and " ++ (show p2') ++
        " have overlapping variables"
checkPattern g typ@(TEither a' b') pat@(PLeft a b p)
  | a == a' && b == b' = do
    RP g' p' <- checkPattern g a $ gp p
    return $ RP g' (PLeft a b (T a p'))
  | otherwise = ERR.throwError $ "Pattern " ++ (show pat) ++
    " is expected to have type " ++ (show typ) ++
      " but appears to have type " ++ (show (TEither a b))
checkPattern g typ@(TEither a' b') pat@(PRight a b p)
  | a == a' && b == b' = do
    RP g' p' <- checkPattern g b $ gp p
    return $ RP g' (PRight a b (T b p'))
  | otherwise = ERR.throwError $ "Pattern " ++ (show pat) ++
    " is expected to have type " ++ (show typ) ++
      " but appears to have type " ++ (show (TEither a b))
checkPattern g t pat@(POr p1 p2) = do
  RP g1 p1' <- checkPattern g t $ gp p1
  RP g2 p2' <- checkPattern g t $ gp p2
  if g1 == g2 then return $ RP g1 (POr (T t p1') (T t p2'))
    else ERR.throwError $ "In Pattern " ++ (show pat) ++
      ", sub-patterns" ++ (show p1') ++ " and " ++ (show p2') ++
        " are expected to have the same set of variables, but do not"
checkPattern _ t p = do
  ERR.throwError $ "Pattern " ++ (show p) ++
    " does not fit with expected type " ++ (show t)

-- exhaustive patterns
exhaustive :: [TPattern] -> Type -> Bool
exhaustive _ _ = True
