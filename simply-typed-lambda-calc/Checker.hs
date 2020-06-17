{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}

module Checker where

import           AST

type Result t = Either String (TExpr t)

checkNatExpr :: UExpr -> Result TyNat
checkNatExpr (XNat n)     = do return $ XNat n
checkNatExpr (XAdd e1 e2) = do
  e1' <- checkNatExpr e1
  e2' <- checkNatExpr e2
  return $ XAdd e1' e2'
checkNatExpr (XCond e1 e2 e3) = do
  e1' <- checkBoolExpr e1
  e2' <- checkNatExpr e2
  e3' <- checkNatExpr e3
  return $ XCond e1' e2' e3'
-- checkNatExpr (XApp e1 e2) = do
--   e1' <- checkArrowExpr e1
--   e2 <- checkNatExpr e2
--   return $ XApp e1' e2'

checkBoolExpr :: UExpr -> Result TyBool
checkBoolExpr (XBool b) = do return $ XBool b

-- need a context...
-- checkArrowExpr :: UExpr -> Result (TyArrow TyNat TyNat)
-- checkArrowExpr (XLam x TyNat e) = do
--   e' <- checkNatExpr e
--   return $ XLam x TyNat e'

-- check :: forall t . SimpType t => UExpr -> Either String (TExpr t)
-- check _ = Right $ XNat Z
