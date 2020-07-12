module AST where

import qualified Data.Map.Strict as DM
import qualified Numeric.Natural as N

type Id = String

type Label = String

-- type synonym
type TypeSyn = String

-- Unit is represented as the empty tuple

-- algebraic data types
type ALG u v = DM.Map u v

type Tuple t = ALG N.Natural t

type Record t = ALG Label t

type Sum t = ALG Label (Maybe t)

data Type =
  TSyn TypeSyn -- type synonym
  | TLam Type Type -- function type
  | TTup (Tuple Type) -- tuple type
  | TRec (Record Type) -- record type
  | TSum (Sum Type) -- sum/variant type

data Pattern =
  PTup (Tuple Pattern)
  | PRec (Record Pattern)
  | PSum TypeSyn Label (Maybe Pattern)

data Expr =
  EVar Id -- variable
  | ELam Pattern Type Expr -- function term
  | EApp Expr Expr -- function application
  | ELet Pattern Expr Expr -- let binding
  | ETup (Tuple Expr) -- tuple expression
  | ETupPrj Expr N.Natural -- tuple projection
  | ERec (Record Expr) -- record expression
  | ERecPrj Expr Label -- record projection
  | ESum TypeSyn Label (Maybe Expr) -- sum/variant expression
  | EMatch Expr [(Pattern,Expr)] -- pattern match
  | ESyn TypeSyn Type Expr -- type synonym definition
