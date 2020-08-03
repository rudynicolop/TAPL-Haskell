{
module Parser(parse) where
import AST
import qualified Lexer as L
}

%name parse
%tokentype { L.Token }
%error { parseError }

%token
unit {L.TUNIT}
'->' {L.ARROW}
'*' {L.PROD}
'+' {L.SUM}
'_' {L.WILD}
'()' {L.UNIT}
',' {L.COMMA}
Left {L.LEFT}
Right {L.RIGHT}
'||' {L.POR}
fun {L.FUN}
':' {L.COLON}
'=>' {L.MAPSTO}
let {L.LET}
'=' {L.LETEQ}
in {L.IN}
fst {L.FST}
snd {L.SND}
match {L.MATCH}
with {L.WITH}
'|' {L.MID}
end {L.END}
'(' {L.LPAREN}
')' {L.RPAREN}
var {L.VAR $$}

%nonassoc '(' ')'
%nonassoc '=>'
%nonassoc fun ':'
%nonassoc Left Right
%left '+' '*' '||'
%nonassoc var unit
%nonassoc let '=' in
%nonassoc match with '|' end
%left ','
%nonassoc fst snd
%left APP
%right '->'
%nonassoc '()'

%%

Expr : '()' {EUnit}
  | var {EName (B $1)}
  | fun Pattern ':' Type '=>' Expr {EFun (B $2) $4 (B $6)}
  | Expr Expr %prec APP {EApp (B $1) (B $2)}
  | let Pattern '=' Expr in Expr {ELet (B $2) (B $4) (B $6)}
  | Expr ',' Expr {EPair (B $1) (B $3)}
  | fst Expr {EFst (B $2)}
  | snd Expr {ESnd (B $2)}
  | Left Type '+' Type Expr {ELeft $2 $4 (B $5)}
  | Right Type '+' Type Expr {ERight $2 $4 (B $5)}
  | match Expr with matchSeq end {EMatch (B $2) $4}
  | '(' Expr ')' {$2} 

matchSeq : matchCase {[$1]}
  | matchCase matchSeq {$1 : $2}

matchCase : '|' Pattern '=>' Expr {(B $2, B $4)}

Pattern : '_' {PBase (B Nothing)}
  | var {PBase (B (Just $1))}
  | Pattern ',' Pattern {PPair (B $1) (B $3)}
  | Left Type '+' Type Pattern {PLeft $2 $4 (B $5)}
  | Right Type '+' Type Pattern {PRight $2 $4 (B $5)}
  | Pattern '||' Pattern {POr (B $1) (B $3)}
  | '(' Pattern ')' {$2}

Type : unit {TUnit}
  | Type '->' Type {TFun $1 $3}
  | Type '*' Type {TPair $1 $3}
  | Type '+' Type {TEither $1 $3}
  | '(' Type ')' {$2}

{
parseError :: [L.Token] -> a
parseError token = error $ "unexpected token " ++ (show token)
}
