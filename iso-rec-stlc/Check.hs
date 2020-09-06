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
check g efold@(EFold t e) = ERR.throwError "incomplete"
check _ _     = ERR.throwError "unimplemented"
