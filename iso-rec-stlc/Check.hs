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
      | t11 == t2 = return $ TA t12 $ EApp (TA (TFun t11 t12) e1') (TA t12 e2')
      | otherwise = ERR.throwError $
        "In application " ++ (show eapp) ++ ", argument " ++ (show e2') ++
          " is expected to have type " ++ (show t11) ++
            ", but has type " ++ (show t2)
    checkapp (TA t1 e1') e2' = ERR.throwError $
      "In application " ++ (show eapp) ++
        ", expression " ++ (show e1') ++ " has type " ++ (show t1)
check g eunfold@(EUnfold t e) =
  case gt t of
    TRec a r -> do
      TA t' e' <- check g $ ge e
      if alphaEq t' (TRec a r)
        then return $ TA (unfold a r) $ EUnfold (TR a r) (TA t' e')
        else ERR.throwError $ "In unfold-expression " ++ (show eunfold) ++
          ", sub-expression " ++ (show e') ++ " is expected to have type " ++
            (show t) ++ ", but has type " ++ (show t')
    wt -> ERR.throwError $ "In unfold-expression " ++ (show eunfold) ++
        ", type argument " ++ (show wt) ++ " should be a recursive type"
check g efold@(EFold t e) = ERR.throwError "unimplemented"

-- type variable capture-avoiding substitution
-- invariant: type passed in is a recursive type
-- unfold X t= t{mu X. t/X}
unfold :: Id -> Type -> Type
unfold _ TUnit = TUnit
unfold x (TFun t1 t2) = TFun (unfold x t1) (unfold x t2)
unfold x (TVar y)
  -- weird but makes sense...I guess...?
  | y == x = TRec x (TVar x)
  | otherwise = TVar y
unfold x (TRec a r)
  -- strange case...
  | a == x = TRec a r
  -- also strange
  | otherwise = TRec a $ unfold x r
