{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module AST where

type Id = String

data Type =
  TUnit
  | TFun Type Type
  | TVar Id
  | TRec Id Type
  | TPrd Type Type
  | TSum Type Type
  deriving (Eq)

instance Show Type where
  show TUnit      = "unit"
  show (TFun a b) = "(" ++ (show a) ++ " -> " ++ (show b) ++ ")"
  show (TVar x)   = x
  show (TRec x t) = "(μ " ++ x ++ ". " ++ (show t) ++ ")"
  show (TPrd a b) = "(" ++ (show a) ++ " * " ++ (show b) ++ ")"
  show (TSum a b) = "(" ++ (show a) ++ " + " ++ (show b) ++ ")"

data Expr k r =
  EUnit
  | EVar (k Id)
  | EFun Id Type (k (Expr k r))
  | EApp (k (Expr k r)) (k (Expr k r))
  | EFold r (k (Expr k r))
  | EUnfold r (k (Expr k r))
  | ELet Id (k (Expr k r)) (k (Expr k r))
  | EPrd (k (Expr k r)) (k (Expr k r))
  | EFst (k (Expr k r))
  | ESnd (k (Expr k r))
  | ELeft Type (k (Expr k r))
  | ERight Type (k (Expr k r))
  | ECase (k (Expr k r)) (k (Expr k r)) (k (Expr k r))

instance Eq (t (Expr t r)) => Eq (Expr t r) where
    (==) = (==)

-- untyped fold/unfold type
data UR = UR Type
  deriving (Eq)

-- typed fold/unfold type
data TR = TR Id Type
  deriving (Eq)

instance Show UR where
  show (UR t) = show t

instance Show TR where
  show (TR x t) = show (TRec x t)

class Show r => RecursiveType r where
  gt :: r -> Type

instance RecursiveType UR where
  gt (UR t) = t

instance RecursiveType TR where
  gt (TR x t) = TRec x t

-- untyped annotation
data UA e = UA e
  deriving (Eq)

instance Show e => Show (UA e) where
  show (UA e) = show e

-- type annotation
data TA e = TA Type e
  deriving (Eq)

instance Show e => Show (TA e) where
  show (TA _ e) = show e

type UExpr = Expr UA UR

type TExpr = Expr TA TR

class Annotation t where
  ge :: t (Expr t r) -> Expr t r
  gx :: t (Id) -> Id

instance Annotation UA where
  ge (UA e) = e
  gx (UA x) = x

instance Annotation TA where
  ge (TA _ e) = e
  gx (TA _ x) = x

instance (Annotation t, RecursiveType r, Show (t (Expr t r))) => Show (Expr t r) where
  show EUnit = "()"
  show (EVar x) = gx x
  show (EFun x t e) = "(λ " ++ x ++ ": " ++ (show t) ++ ". " ++ (show e) ++ ")"
  show (EApp e1 e2) = "(" ++ (show e1) ++ " " ++ (show e2) ++ ")"
  show (EFold t e) = "(fold [" ++ (show t) ++ "] " ++ (show e) ++ ")"
  show (EUnfold t e) = "(unfold [" ++ (show t) ++ "] " ++ (show e) ++ ")"
  show (ELet x e1 e2) =
    "(let " ++ x ++ " = " ++ (show e1) ++ " in " ++ (show e2) ++ ")"
  show (EPrd e1 e2) = "(" ++ (show e1) ++ "," ++ (show e2) ++ ")"
  show (EFst e) = "(fst " ++ (show e) ++  ")"
  show (ESnd e) = "(snd " ++ (show e) ++  ")"
  show (ELeft t e) = "(Left " ++ (show t) ++ " " ++ (show e) ++ ")"
  show (ERight t e) = "(Right " ++ (show t) ++ " " ++ (show e) ++ ")"
  show (ECase e e1 e2) =
    "(case " ++ (show e) ++ " of " ++ (show e1) ++ " | " ++ (show e2) ++ ")"

class AlphaEq t where
  alphaEq :: t -> t -> Bool
  -- subvar x y t substitues type variable y for x in t
  subvar :: Id -> Id -> t -> t

instance AlphaEq Type where
  alphaEq TUnit TUnit               = True
  alphaEq (TFun a1 b1) (TFun a2 b2) = alphaEq a1 a2 && alphaEq b1 b2
  alphaEq (TVar x1) (TVar x2)       = x1 == x2
  alphaEq (TRec a1 r1) (TRec a2 r2) = alphaEq r2 $ subvar a1 a2 r1
  alphaEq (TPrd a1 b1) (TPrd a2 b2) = alphaEq a1 a2 && alphaEq b1 b2
  alphaEq (TSum a1 b1) (TSum a2 b2) = alphaEq a1 a2 && alphaEq b1 b2
  alphaEq _ _                       = False

  subvar a1 a2 t = subvar' t
    where
      subvar' :: Type -> Type
      subvar' TUnit = TUnit
      subvar' (TFun t1 t2) = TFun (subvar' t1) (subvar' t2)
      subvar' (TVar x)
        | x == a1 = TVar a2
        | otherwise = TVar x
      subvar' (TRec a r)
        | a == a1 = TRec a r
        | otherwise = TRec a $ subvar' r
      subvar' (TPrd a b) = TPrd (subvar' a) (subvar' b)
      subvar' (TSum a b) = TSum (subvar' a) (subvar' b)
