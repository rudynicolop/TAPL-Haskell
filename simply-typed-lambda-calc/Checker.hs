module Checker where

import           AST
-- import qualified Control.Monad.Except as ERR
import qualified Data.Map.Strict as M

type Gamma = M.Map Id Type

data R = R Type TExpr

type Result = Maybe R

check :: Gamma -> BExpr -> Result
check g (ENat n)  = do return $ R TNat $ ENat n
check g (EBool b) = do return $ R TBool $ EBool b
check g (EVar (B x)) = do
  t <- M.lookup x g
  return $ R t $ EVar $ T t x
check g (EAdd (B e1) (B e2)) = checknat g EAdd e1 e2
check g (EMul (B e1) (B e2)) = checknat g EMul e1 e2
check g (ESub (B e1) (B e2)) = checknat g ESub e1 e2

checknat :: Gamma -> (T TExpr -> T TExpr -> TExpr) -> BExpr -> BExpr -> Result
checknat g' c e1' e2' = do
  e1'' <- check g' e1'
  e2'' <- check g' e2'
  case (e1'', e2'') of
    (R TNat e1''', R TNat e2''') -> do return $ R TNat $ c (T TNat e1''') (T TNat e2''')
    _                            -> Nothing
