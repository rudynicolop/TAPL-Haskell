module Eval(eval) where

import           AST
import           Control.Monad.Except


natSucc :: Nat -> Nat
natSucc n = S n

natPred :: Nat -> Nat
natPred Z     = Z
natPred (S n) = n

isZero :: Nat -> VB
isZero Z     = VTrue
isZero (S _) = VFalse

eval :: Term -> Either String Val
eval TTrue = return $ VBool VTrue
eval TFalse = return $ VBool VFalse
eval Zero   = return $ VNum Z
eval (IsZero t) = do
    vm <- eval t;
    case vm of
        VNum n  -> return $ VBool $ isZero n
        VBool _ -> throwError "iszero does not accept boolean arguments"
eval (Succ t) = do
    vm <- eval t;
    case vm of
         VNum n  -> return $ VNum $ natSucc n
         VBool _ -> throwError "successor does not accept boolean arguments"
eval (Pred t) = do
     vm <- eval t;
     case vm of
          VNum n -> return $ VNum $ natPred n
          VBool _ -> throwError "predecessor does not accept boolean arguments"
eval (IfThenElse t1 t2 t3) = do
      vm <- eval t1;
      case vm of
           VNum _       -> throwError "conditional guard must be boolean"
           VBool VTrue  -> eval t2
           VBool VFalse -> eval t3
