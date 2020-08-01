{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module AST where

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

data Pattern t =
  PWild
  | PName (t Id)
  | PUnit
  | PPair (t (Pattern t)) (t (Pattern t))
  | PLeft Type Type (t (Pattern t))
  | PRight Type Type (t (Pattern t))

instance Eq (t (Pattern t)) => Eq (Pattern t) where
    (==) = (==)

data Expr t =
  EName (t Id)
  | EUnit
  | EFun (t (Pattern t)) Type (t (Expr t))
  | EApp (t (Expr t)) (t (Expr t))
  | ELet (t (Pattern t)) (t (Expr t)) (t (Expr t))
  | EPair (t (Expr t)) (t (Expr t))
  | EFst (t (Expr t))
  | ESnd (t (Expr t))
  | ELeft Type Type (t (Expr t))
  | ERight Type Type (t (Expr t))
  | EMatch (t (Expr t)) [((t (Pattern t)),(t (Expr t)))]

instance Eq (t (Expr t)) => Eq (Expr t) where
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

type BPattern = Pattern B

type BExpr = Expr B

type TPattern = Pattern T

type TExpr = Expr T

class Annotation t where
  gx :: t (Id) -> Id
  gp :: t (Pattern t) -> Pattern t
  ge :: t (Expr t) -> Expr t

instance Annotation B where
  gx (B x) = x
  gp (B p) = p
  ge (B e) = e

instance Annotation T where
  gx (T _ x) = x
  gp (T _ p) = p
  ge (T _ e) = e

instance (Annotation t, Show (t (Pattern t))) => Show (Pattern t) where
  show PWild          = "_"
  show (PName x)      = gx x
  show PUnit          = "()"
  show (PPair p1 p2)  = "(" ++ (show p1) ++ ", " ++ (show p2) ++ ")"
  show (PLeft a b p)  = "(Left " ++ (show (TEither a b)) ++ (show p) ++ ")"
  show (PRight a b p) = "(Right " ++ (show (TEither a b)) ++ (show p) ++ ")"

instance (Annotation t, Show (t (Pattern t)), Show (t (Expr t))) => Show (Expr t) where
  show (EName x) = gx x
  show EUnit = "()"
  show (EFun p t e) = "(fun " ++ (show p) ++ " : " ++ (show t) ++ " => " ++ (show e) ++ ")"
  show (EApp e1 e2) = "(" ++ (show e1) ++ " " ++ (show e2) ++ ")"
  show (ELet p e1 e2) = "(let " ++ (show p) ++ " = " ++ (show e1) ++ " in" ++ (show e2) ++ ")"
  show (EPair e1 e2) = "(" ++ (show e1) ++ ", " ++ (show e2) ++ ")"
  show (EFst e) = "(fst " ++ (show e) ++ ")"
  show (ESnd e) = "(snd " ++ (show e) ++ ")"
  show (ELeft a b e) = "(Left " ++ (show (TEither a b)) ++ (show e) ++ ")"
  show (ERight a b e) = "(Right " ++ (show (TEither a b)) ++ (show e) ++ ")"
  show (EMatch e cases) =
    foldl (\ acc (p',e') -> acc ++ "\n| " ++ (show p') ++ " => " ++ (show e'))
      ("\nmatch " ++ (show e) ++ " with ") cases
