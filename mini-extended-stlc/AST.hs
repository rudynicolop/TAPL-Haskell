{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module AST where

import qualified Data.Map.Strict as M

type Id = String

data Type =
  TUnit
  | TFun Type Type
  | TPair Type Type
  | TEither Type Type
  deriving (Eq)

instance Show Type where
  show TUnit         = "unit"
  show (TFun a b)    = "(" ++ (show a) ++ " -> " ++ (show b) ++ ")"
  show (TPair a b)   = "(" ++ (show a) ++ " * " ++ (show b) ++ ")"
  show (TEither a b) = "(" ++ (show a) ++ " + " ++ (show b) ++ ")"

data Pattern pb t =
  PBase (t pb)
  | PUnit
  | PPair (t (Pattern pb t)) (t (Pattern pb t))
  | PLeft Type Type (t (Pattern pb t))
  | PRight Type Type (t (Pattern pb t))
  | POr (t (Pattern pb t)) (t (Pattern pb t))

instance Eq (t (Pattern pb t)) => Eq (Pattern pb t) where
    (==) = (==)

type PName = Maybe Id

type Pat t = Pattern PName t

data Expr t =
  EName (t Id)
  | EUnit
  | EFun (t (Pat t)) Type (t (Expr t))
  | EApp (t (Expr t)) (t (Expr t))
  | ELet (t (Pat t)) (t (Expr t)) (t (Expr t))
  | EPair (t (Expr t)) (t (Expr t))
  | EFst (t (Expr t))
  | ESnd (t (Expr t))
  | ELeft Type Type (t (Expr t))
  | ERight Type Type (t (Expr t))
  | EMatch (t (Expr t)) [((t (Pat t)),(t (Expr t)))]

instance Eq (t (Expr t)) => Eq (Expr t) where
    (==) = (==)

type Bindings t = M.Map Id (t (Value t))

data Value t =
  VUnit
  | VClosure  (Bindings t) (t (Pat t)) Type (t (Expr t))
  | VPair (t (Value t)) (t (Value t))
  | VLeft Type Type (t (Value t))
  | VRight Type Type (t (Value t))

instance Eq (t (Value t)) => Eq (Value t) where
    (==) = (==)

-- bare/blank annotation
data B e = B e
  deriving (Eq)

instance Show e => Show (B e) where
  show (B e) = show e

-- type annotation
data T e = T Type e
  deriving (Eq)

instance Show e => Show (T e) where
  show (T _ e) = show e

type BPattern = Pat B

type BExpr = Expr B

type TPattern = Pat T

type TExpr = Expr T

type TBinds = Bindings T

type TValue = Value T

class Annotation t where
  gx :: t (Id) -> Id
  gp :: t (Pat t) -> Pat t
  ge :: t (Expr t) -> Expr t
  gv :: t (Value t) -> Value t
  gopt :: t PName -> PName

instance Annotation B where
  gx (B x) = x
  gp (B p) = p
  ge (B e) = e
  gv (B v) = v
  gopt (B o) = o

instance Annotation T where
  gx (T _ x) = x
  gp (T _ p) = p
  ge (T _ e) = e
  gv (T _ v) = v
  gopt (T _ o) = o

instance (Annotation t, Show (t (Pat t))) => Show (Pat t) where
  show (PBase pb) =
    case gopt pb of
      Nothing  -> "_"
      (Just x) -> x
  show PUnit            = "()"
  show (PPair p1 p2)    = "(" ++ (show p1) ++ ", " ++ (show p2) ++ ")"
  show (PLeft a b p)    = "(Left " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show p) ++ ")"
  show (PRight a b p)   = "(Right " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show p) ++ ")"
  show (POr p1 p2)      = "(" ++ (show p1) ++ " | " ++ (show p2) ++ ")"

instance (Annotation t, Show (t (Pat t)), Show (t (Expr t))) => Show (Expr t) where
  show (EName x) = gx x
  show EUnit = "()"
  show (EFun p t e) = "(fun " ++ (show p) ++ " : " ++ (show t) ++ " => " ++ (show e) ++ ")"
  show (EApp e1 e2) = "(" ++ (show e1) ++ " " ++ (show e2) ++ ")"
  show (ELet p e1 e2) = "(let " ++ (show p) ++ " = " ++ (show e1) ++ " in\n" ++ (show e2) ++ ")"
  show (EPair e1 e2) = "(" ++ (show e1) ++ ", " ++ (show e2) ++ ")"
  show (EFst e) = "(fst " ++ (show e) ++ ")"
  show (ESnd e) = "(snd " ++ (show e) ++ ")"
  show (ELeft a b e) = "(Left " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show e) ++ ")"
  show (ERight a b e) = "(Right " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show e) ++ ")"
  show (EMatch e cases) =
    (foldl (\ acc (p',e') -> acc ++ "\n| " ++ (show p') ++ " => " ++ (show e'))
      ("match " ++ (show e) ++ " with ") cases) ++ "\nend"

instance (Annotation t, Show (t (Pat t)), Show (t (Expr t)), Show (t (Value t))) => Show (Value t) where
  show VUnit = "()"
  show (VClosure _ p t e) = "(fun " ++ (show p) ++ " : " ++ (show t) ++ " => " ++ (show e) ++ ")"
  show (VPair v1 v2) = "(" ++ (show v1) ++ ", " ++ (show v2) ++ ")"
  show (VLeft a b v) = "(Left " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show v) ++ ")"
  show (VRight a b v) = "(Right " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show v) ++ ")"
