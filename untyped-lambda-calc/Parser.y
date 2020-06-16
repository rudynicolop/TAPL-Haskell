{

module Parser(parse) where
import AST
import qualified Macros as M

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
nat {NAT $$}
succ {SUCC}
id {ID}
add {ADD}
mult {MULT}
pred {PRED}
sub {SUB}
true {TRUE}
false {FALSE}
cond {COND}
and {AND}
or {OR}
iszero {ISZERO}
eq {AST.EQ}
Y {Y}

%nonassoc var lparen lambda nat succ id add mult pred sub true false cond and or iszero eq Y
%nonassoc APP

%%

Expr : var {Var $1}
  | lambda var dot Expr {Lam $2 $4}
  | Expr Expr %prec APP {App $1 $2}
  | lparen Expr rparen {$2}
  | nat {M.makeNat $1}
  | succ {M.mSucc}
  | id {M.mId}
  | add {M.mAdd}
  | mult {M.mMult}
  | pred {M.mPred}
  | sub {M.mSub}
  | true {M.mTrue}
  | false {M.mFalse}
  | cond {M.mCond}
  | and {M.mAnd}
  | or {M.mOr}
  | iszero {M.mIsZero}
  | eq {M.mEq}
  | Y {M.mY}
{

parseError :: [Token] -> a
parseError token = token |> show |> (++) "unexpected token " |> error

}
