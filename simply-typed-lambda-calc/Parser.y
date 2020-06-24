{

module Parser(parse) where
import AST
import qualified Macros as M
import qualified Lexer as L
}

%name parse
%tokentype { L.Token }
%error { parseError }

%token
nat {L.NNUM $$}
natType {L.NATTYPE}
bulType {L.BULTYPE}
true {L.TRUE}
false {L.FALSE}
'0' {L.ZERO}
'S' {L.SUCC}
'=>' {L.MAPSTO}
'->' {L.ARROW}
'!' {L.NOT}
'+' {L.ADD}
'*' {L.MUL}
'-' {L.SUB}
'=' {L.TEQ}
'<' {L.TLE}
'&' {L.AND}
'|' {L.OR}
':' {L.HASTYPE}
fun {L.LAMBDA}
if {L.IF}
then {L.THEN}
else {L.ELSE}
eof {L.EOF}
'(' {L.LPAREN}
')' {L.RPAREN}
var {L.VAR $$}

%nonassoc '(' ')'
%nonassoc '=>'
%nonassoc fun ':'
%nonassoc nat natType bulType true false '0' eof var
%nonassoc if then else
%nonassoc '|'
%nonassoc '&'
%nonassoc '=' '<'
%nonassoc '!'
%left '+' '-'
%left '*'
%nonassoc S
%left APP
%right '->'

%%

Expr : nat {ENat $ M.genNat $1}
  | true {EBul T}
  | false {EBul F}
  | '0' {ENat Z}
  | var {EVar $ B $1}
  | '!' Expr {ENot $ B $2}
  | Expr '+' Expr {EBOp EAdd (B $1) (B $3)}
  | Expr '*' Expr {EBOp EMul (B $1) (B $3)}
  | Expr '-' Expr {EBOp ESub (B $1) (B $3)}
  | Expr '=' Expr {EBOp EEq (B $1) (B $3)}
  | Expr '<' Expr {EBOp ELe (B $1) (B $3)}
  | Expr '&' Expr {EBOp EAnd (B $1) (B $3)}
  | Expr '|' Expr {EBOp EOr (B $1) (B $3)}
  | if Expr then Expr else Expr {ECond (B $2) (B $4) (B $6)}
  | fun var ':' Type '=>' Expr {ELam $2 $4 (B $6)}
  | Expr Expr %prec APP {EApp (B $1) (B $2)}
  | '(' Expr ')' {$2}

Type : natType {TNat}
  | bulType {TBul}
  | Type '->' Type {TArrow $1 $3}
  | '(' Type ')' {$2}
{

parseError :: [L.Token] -> a
parseError token = token |> show |> (++) "unexpected token " |> error
}
