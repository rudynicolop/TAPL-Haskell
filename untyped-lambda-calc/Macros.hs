module Macros where

import           AST

sx :: String
sx = "x"

x :: Expr
x = Var sx

sy :: String
sy = "y"

y :: Expr
y = Var sy

sf :: String
sf = "f"

f :: Expr
f = Var sf

sg :: String
sg = "g"

g :: Expr
g = Var sg

sh :: String
sh = "h"

h :: Expr
h = Var sh

sn :: String
sn = "n"

n :: Expr
n = Var sn

mId :: Expr
mId = Lam sn x

mSucc :: Expr
mSucc = Lam sn $ Lam sf $ Lam sx $ App f $ App (App n f) x

makeNat :: Integer -> Expr
makeNat = Lam sf $ Lam sx $ makeNatHelp
  where
    makeNatHelp :: Integer -> Expr
    makeNatHelp 0 = x
    makeNatHelp i = App f $ makeNatHelp $ i-1

mAdd :: Expr
mAdd = Lam sx $ Lam sy $ App (App x mSucc) y

mMult :: Expr
mMult = Lam sx $ Lam sy $ App (App x $ App mAdd y) mId

mPred :: Expr
mPred = Lam sn $ Lam sf $ Lam sx $ App (App (App n $ Lam sg $ Lam sh $ App h $ App g f) Lam "u" x) mId

mSub :: Expr
mSub = Lam sx $ Lam sy $ App (App y mPred) x

mTrue :: Expr
mTrue = Lam sx $ Lam sy $ x

mFalse :: Expr
mFalse = Lam sx $ Lam sy $ y

mCond :: Expr
mCond = Lam sf $ Lam sx $ Lam sy $ App (App f x) sy

mAnd :: Expr
mAnd = Lam sx $ Lam sy $ App (App sx sy) mFalse

mOr :: Expr
mOr = Lam sx $ Lam sy $ App (App sx mTrue) sy

mIsZero :: Expr
mIsZero = Lam sn $ App (App n $ Lam sx mFalse) mTrue

mEq :: Expr
mEq = Lam sx $ Lam sy $ App (App mAnd $ App mIsZero $ App (App mSub y) x) App mIsZero $ App (App mSub x) y

mY :: Expr
mY = Lam sf $ App (Lam sx $ App f $ App x x) (Lam sx $ App f $ App x x)
