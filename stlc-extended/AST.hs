module AST where

import qualified Data.Map.Strict as DM

type Id = String

-- natural numbers
data Nat = Z | S Nat

-- booleans
data Bul = T | F

-- Unit is represented as the empty tuple

-- algebraic data types
type ALG u v = DM.Map u v

type Tuple t = ALG Nat t

type Record t = ALG Id t

type Sum t = ALG Id t

data Type =
  TNat -- natural number type
  | TBul -- boolean type
  | TLam Type Type -- function type
  | TTup (Tuple Type) -- tuple type
  | TRec (Record Type) -- record type
  | TSum (Sum Type) -- sum/variant type

data Value =
  VNat Nat -- natural number term
  | VBul Bul -- boolean term
  | VLam Id Type Expr -- function term
  | VTup [Value] -- tuple value
  | VRec (Record Value) -- record value
  | VSum (Sum Type) Id Value -- sum/variant value

-- binary operations
data Op = Add | Eq | And

data Expr =
  EVar Id
  | EValue Value -- values subset expressions
  | EBOp Op Expr Expr -- binary operations
  | ECnd Expr Expr Expr -- conditionals
  | EApp Expr Expr -- function application
  | ESeq Expr Expr -- sequences
  | EAsc Type Expr -- ascription
  | ELet Id Expr Expr -- let binding
  | ETup [Expr] -- tuple expression
  | ETupPrj Expr Nat -- tuple projection
  | ERec (Record Expr) -- record expression
  | ERecPrj Expr Id -- record projection
  | ESum (Sum Type) Id Expr -- sum/variant expression
  | EMatch Expr (Sum Expr) -- match expression
