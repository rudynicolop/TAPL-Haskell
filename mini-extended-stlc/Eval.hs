{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Eval where

import           AST
import qualified Check    as C
import qualified Data.Set as S

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
