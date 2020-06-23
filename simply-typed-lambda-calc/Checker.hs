module Checker(check,R(..)) where

import           AST
import qualified Control.Monad.Except as ERR
import qualified Data.Map.Strict      as M

type Gamma = M.Map Id Type

data R = R Type TExpr

type Result = Either String R

check :: (Show (t (Expr t)), Annotation t) => Gamma -> Expr t -> Result
check g (ENat n)  = do return $ R TNat $ ENat n
check g (EBul b) = do return $ R TBul $ EBul b
check g (EVar tx) = do
  let x = gx tx in do
    case M.lookup x g of
      Nothing -> ERR.throwError $ "Unbound variable " ++ x
      Just t  -> return $ R t $ EVar $ Ty t x
check g bang@(ENot e) = do
  e' <- e |> ge |> check g
  case e' of
    R TBul e'' -> do return $ R TBul $ ENot $ Ty TBul e''
    R t _         -> do
      ERR.throwError $
        "In expression " ++ (show bang) ++
          ", expression " ++ (show e) ++ " has type " ++ (show t)
check g op@(EAdd e1 e2) = checkbi g TNat TNat EAdd e1 e2 op
check g op@(EMul e1 e2) = checkbi g TNat TNat EMul e1 e2 op
check g op@(ESub e1 e2) = checkbi g TNat TNat ESub e1 e2 op
check g op@(EEq  e1 e2) = checkbi g TNat TBul EEq e1 e2 op
check g op@(ELe  e1 e2) = checkbi g TNat TBul ELe e1 e2 op
check g op@(EOr  e1 e2) = checkbi g TBul TBul EOr e1 e2 op
check g op@(EAnd e1 e2) = checkbi g TBul TBul EAnd e1 e2 op
check g cond@(ECond e1 e2 e3) = do
  e1' <- e1 |> ge |> check g
  e2' <- e2 |> ge |> check g
  e3' <- e3 |> ge |> check g
  checkcond e1' e2' e3'
  where
    checkcond :: R -> R -> R -> Result
    checkcond (R TBul e1'') (R t2 e2'') (R t3 e3'')
      | t2 == t3 = return $ R t2 $ ECond (Ty TBul e1'') (Ty t2 e2'') (Ty t3 e3'')
      | otherwise = ERR.throwError $
        "In conditional " ++ (show cond) ++
          ", then-clause " ++ (show e2'') ++ " has type " ++ (show t2) ++
            ", but else-clause " ++ (show e3'') ++ " has type " ++ (show t3)
    checkcond (R t1 _) _ _ = ERR.throwError $
        "Guard expression " ++
          (show e1) ++ ", in conditional " ++ (show cond) ++
            ", has type " ++ (show t1)
check g (ELam x t e) = do
  R t' e' <- e |> ge |> check (M.insert x t g)
  return $ R (TArrow t t') (ELam x t $ Ty t' e')
check g app@(EApp e1 e2) = do
  e1' <- e1 |> ge |> check g
  e2' <- e2 |> ge |> check g
  checkapp e1' e2'
  where
    checkapp :: R -> R -> Result
    checkapp (R (TArrow t1 t3) e1') (R t2 e2')
      | t1 == t2 = return $ R t3 $ EApp (Ty (TArrow t1 t3) e1') (Ty t1 e2')
      | otherwise = ERR.throwError $
        "In application " ++ (show app) ++ ", argument " ++ (show e2') ++
          " is expected to have type " ++ (show t1) ++
            ", but has type " ++ (show t2)
    checkapp (R t1 _) _ = ERR.throwError $
      "In application " ++ (show app) ++
        ", expression " ++ (show e1) ++ " has type " ++ (show t1)

checkbi :: (Show (t (Expr t)), Annotation t) => Gamma -> Type -> Type -> (Ty TExpr -> Ty TExpr -> TExpr) -> t (Expr t) -> t (Expr t) -> Expr t -> Result
checkbi g t rt c e1 e2 op = do
  e1' <- e1 |> ge |> check g
  e2' <- e2 |> ge |> check g
  checkbi' e1' e2'
    where
      checkbi' :: R -> R -> Result
      checkbi' (R t1 e1'') (R t2 e2'')
        | (t == t1 && t == t2) = return $ R rt $ c (Ty t e1'') (Ty t e2'')
        | otherwise = ERR.throwError $
          "In expression " ++ (show op) ++
            ", expression " ++ (show e1'') ++ " has type " ++ (show t1) ++
              ", but expression " ++ (show e2'') ++ " has type " ++ (show t2)
