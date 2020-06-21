module Macros where

import           AST

genNat :: Integer -> Nat
genNat 0 = Z
genNat n = S $ genNat $ n-1
