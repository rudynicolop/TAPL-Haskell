module Eval(eval) where

import AST
import Control.Monad.Except

eval :: Term -> Either String Val
eval TTrue = return $ VBool True
eval TFalse = return $ VBool False
eval Zero   = return $ VNum 0
eval (IsZero t) = do
    vm <- eval t;
    case vm of
        VNum n -> return $ VBool $ n == 0
        VBool _ -> throwError "iszero does not accept boolean arguments"
eval (Succ t) = do
    vm <- eval t;
    case vm of
         VNum n -> return $ VNum $ n + 1
         VBool _ -> throwError "successor does not accept boolean arguments"
eval (Pred t) = do
     vm <- eval t;
     case vm of
          VNum n -> return $ VNum $ n - 1
          VBool _ -> throwError "predecessor does not accept boolean arguments"
eval (IfThenElse t1 t2 t3) = do
      vm <- eval t1;
      case vm of
           VNum _ -> throwError "conditional guard must be boolean"
           VBool True -> eval t2
           VBool False -> eval t3
