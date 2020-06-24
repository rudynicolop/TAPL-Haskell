{-# LANGUAGE UnicodeSyntax #-}

module AST where

(|>) x f = f x

data Token =
  VAR String
  | LAMBDA
  | DOT
  | LPAREN
  | RPAREN
  | NAT Integer
  | SUCC
  | ID
  | ADD
  | MULT
  | PRED
  | SUB
  | TRUE
  | FALSE
  | COND
  | AND
  | OR
  | ISZERO
  | EQ
  | Y
  deriving (Show)

data Expr =
  Var String
  | Lam String Expr
  | App Expr Expr

instance Show Expr where
  show (Var x)     = x
  show (Lam x e)   = "(λ" ++ x ++ "." ++ (show e) ++ ")"
  show (App e1 e2) = "(" ++ (show e1) ++ " " ++ (show e2) ++ ")"
