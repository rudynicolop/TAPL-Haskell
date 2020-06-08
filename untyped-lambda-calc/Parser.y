{

module Parser(parse) where
import AST


}

%name parse
%tokentype { Token }
%error { parseError }

%token
var {VAR $$}
lambda {LAMBDA}
dot {DOT}
lparen {LPAREN}
rparen {RPAREN}

%nonassoc var lparen lambda
%nonassoc APP

%%

Expr : var {Var $1}
  | lambda var dot Expr {Lam $2 $4}
  | Expr Expr %prec APP {App $1 $2}
  | lparen Expr rparen {$2}

{

parseError :: [Token] -> a
parseError token = token |> show |> (++) "unexpected token " |> error

}
