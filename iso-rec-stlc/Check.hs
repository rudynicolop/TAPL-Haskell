{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Check where

import           AST
import qualified Control.Monad        as CM
import qualified Control.Monad.Except as ERR
import qualified Data.Map.Strict      as M
import qualified Data.Set             as S

type Gamma = M.Map Id Type

type Result = Either String (TA TExpr)

check :: (Annotation t, RecursiveType r, Show (t (Expr t r)))
  => Gamma -> Expr t r -> Result
check g EUnit = return $ TA TUnit EUnit
check g (EVar ax) = do
  let x = gx ax in do
    case M.lookup x g of
      Nothing -> ERR.throwError $ "Unbound variable " ++ x
      Just t  -> return $ TA t $ EVar $ TA t x
check g (EFun x t e) = do
  TA t' e' <- check (M.insert x t g) (ge e)
  return $ TA (TFun t t') $ (EFun x t (TA t' e'))
check g eapp@(EApp e1 e2) = do
  e1' <- check g (ge e1)
  e2' <- check g (ge e2)
  checkapp e1' e2'
  where
    checkapp :: TA TExpr -> TA TExpr -> Result
    checkapp (TA (TFun t11 t12) e1') (TA t2 e2')
      | alphaEq t11 t2 = return $ TA t12 $ EApp (TA (TFun t11 t12) e1') (TA t12 e2')
      | otherwise = ERR.throwError $
        "In application " ++ (show eapp) ++ "\nargument " ++ (show e2') ++
          "\nis expected to have type " ++ (show t11) ++
            "\nbut has type " ++ (show t2)
    checkapp (TA t1 e1') e2' = ERR.throwError $
      "In application " ++ (show eapp) ++
        "\nexpression " ++ (show e1') ++ "\nhas type " ++ (show t1)
check g eunfold@(EUnfold t e) =
  case gt t of
    TRec a r -> do
      eprime@(TA t' e') <- check g $ ge e
      if alphaEq t' (TRec a r)
        then return $ TA (unfold a r) $ EUnfold (TR a r) eprime
        else ERR.throwError $ "In unfold-expression " ++ (show eunfold) ++
          "\nsub-expression " ++ (show e') ++ "\nis expected to have type " ++
            (show t) ++ "\nbut has type " ++ (show t')
    wt -> ERR.throwError $ "In unfold-expression " ++ (show eunfold) ++
        "\ntype argument " ++ (show wt) ++ "\nshould be a recursive type."
check g efold@(EFold t e) =
  case gt t of
    trec@(TRec a r) -> do
      eprime@(TA t' e') <- check g $ ge e
      let uar = unfold a r in
        if alphaEq t' uar
          then return $ TA trec $ EFold (TR a r) eprime
          else ERR.throwError $ "In fold-expression " ++ (show efold) ++
            "\nsub-expression " ++ (show e') ++ "\nis expected to have type " ++
              (show uar) ++ "\nbut has type " ++ (show t')
    wt -> ERR.throwError $ "In fold-expression " ++ (show efold) ++
        "\ntype argument " ++ (show wt) ++ "\nshould be a recursive type."
check g (ELet x e1 e2) = do
  e1'@(TA t1 _) <- check g $ ge e1
  e2'@(TA t2 _) <- check (M.insert x t1 g) $ ge e2
  return $ TA t2 $ ELet x e1' e2'
check g (EPrd e1 e2) = do
  e1'@(TA t1 _) <- check g $ ge e1
  e2'@(TA t2 _) <- check g $ ge e2
  return $ TA (TPrd t1 t2) $ EPrd e1' e2'
check g efst@(EFst e) = do
  e'@(TA t _) <- check g $ ge e
  case t of
    TPrd t1 t2 -> return $ TA t1 $ EFst e'
    _ -> ERR.throwError $ "In expression " ++ (show efst) ++
      "\nsub-expression " ++ (show e') ++ "\nhas type " ++ (show t)
check g esnd@(ESnd e) = do
  e'@(TA t _) <- check g $ ge e
  case t of
    TPrd t1 t2 -> return $ TA t2 $ ESnd e'
    _ -> ERR.throwError $ "In expression " ++ (show esnd) ++
      "\nsub-expression " ++ (show e') ++ "\nhas type " ++ (show t)
check g (ELeft b e) = do
  e'@(TA a _) <- check g $ ge e
  return $ TA (TSum a b) $ ELeft b e'
check g (ERight a e) = do
  e'@(TA b _) <- check g $ ge e
  return $ TA (TSum a b) $ ERight b e'
check g ecase@(ECase e e1 e2) = do
  e'@(TA t _) <- check g $ ge e
  case t of
    TSum a b -> do
      e1' <- check g $ ge e1
      e2' <- check g $ ge e2
      caseHelp a b e' e1' e2'
    _ -> ERR.throwError $ "In expression " ++ (show ecase) ++
      "\nsub-expression " ++ (show e') ++ "\nhas type " ++ (show t)
  where
    caseHelp :: Type -> Type -> TA TExpr -> TA TExpr -> TA TExpr -> Result
    caseHelp a b e' e1'@(TA t1'@(TFun a' t1) _) e2'@(TA t2'@(TFun b' t2) _)
      | a == a' && b == b' && t1 == t2 =
        return $ TA t1 $ ECase e' e1' e2'
      | otherwise = ERR.throwError $ "In expression " ++ (show ecase) ++
        "\nsub-expression " ++ (show e1') ++ "\nshould have type "
          ++ (show a) ++ " -> ? \nand sub-expression " ++ (show e2') ++
            "\nshould have type " ++ (show b) ++ " -> ? \nbut respectively they have types "
              ++ (show t1') ++ "\nand " ++ (show t2')
    caseHelp a b e' e1'@(TA t1 _) e2'@(TA t2 _) = ERR.throwError $ "In expression "
      ++ (show ecase) ++ "\nsub-expression " ++ (show e1') ++ "\nshould have type "
        ++ (show a) ++ " -> ? \nand sub-expression " ++ (show e2') ++
          "\nshould have type " ++ (show b) ++ " -> ? \nbut respectively they have types "
            ++ (show t1) ++ "\nand " ++ (show t2)

-- type variable capture-avoiding substitution
-- invariant: type passed in is a recursive type
-- unfold X t = {mu X. t/X}
unfold :: Id -> Type -> Type
unfold x t = unfold' t
  where
    unfold' :: Type -> Type
    unfold' TUnit        = TUnit
    unfold' (TFun t1 t2) = TFun (unfold' t1) (unfold' t2)
    unfold' (TVar y)
      | y == x = TRec x t
      | otherwise = TVar y
    unfold' (TRec a r)
      -- strange case...
      | a == x = TRec a r
      -- also strange
      | otherwise = TRec a $ unfold' r
    unfold' (TPrd a b) = TPrd (unfold' a) (unfold' b)
    unfold' (TSum a b) = TSum (unfold' a) (unfold' b)
