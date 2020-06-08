module AST where

(|>) x f = f x

data Token =
  VAR String
  | LAMBDA
  | DOT
  | LPAREN
  | RPAREN
  deriving (Show)

data Expr =
  Var String
  | Lam String Expr
  | App Expr Expr
  deriving (Show)
