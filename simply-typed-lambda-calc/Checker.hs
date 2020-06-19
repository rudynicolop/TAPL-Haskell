module Checker where

import           AST
import           Data.Map.Strict as M

type Gamma = M.Map Id Type
