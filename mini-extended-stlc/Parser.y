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
%nonassoc var
%nonassoc '=>'
%nonassoc fun ':'
%left '+' '*' '||'
%nonassoc unit
%nonassoc let '=' in LETEXPR
%nonassoc match with '|' end MATCHEXPR
%nonassoc ',' PAIR
%nonassoc fst snd
%nonassoc Left Right
%left APP
%right '->'
%nonassoc '()'

%%

expr :: {BExpr}
expr : let pat '=' funexpr in expr {ELet (B $2) (B $4) (B $6)}
     | funexpr {$1}

funexpr :: {BExpr}
funexpr : fun pat ':' typ '=>' funexpr {EFun (B $2) $4 (B $6)}
      | matchexpr {$1}

matchexpr :: {BExpr}
matchexpr : match expr with matchSeq {EMatch (B $2) $4}
      | pairexpr {$1}

matchSeq :: {[(B BPattern, B BExpr)]}
matchSeq : matchCase end {[$1]}
         | matchCase '|' matchSeq {$1 : $3}

matchCase :: {(B BPattern, B BExpr)}
matchCase : pat '=>' expr {(B $1, B $3)}

pairexpr :: {BExpr}
pairexpr : appexpr ',' appexpr {EPair (B $1) (B $3)}
         | appexpr  {$1}

appexpr :: {BExpr}
appexpr : appexpr projexpr {EApp (B $1) (B $2)}
        | projexpr {$1}

projexpr :: {BExpr}
projexpr : fst projexpr {EFst (B $2)}
         | snd projexpr {ESnd (B $2)}
         | eitherexpr {$1}

eitherexpr :: {BExpr}
eitherexpr : Left typ typ eitherexpr {ELeft $2 $3 (B $4)}
           | Right typ typ eitherexpr {ERight $2 $3 (B $4)}
           | bottomexpr {$1}

bottomexpr :: {BExpr}
bottomexpr : '()' {EUnit}
        | var {EName (B $1)}
        | '(' expr ')' {$2}

pat :: {BPattern}
pat : pat '||' pat {POr (B $1) (B $3)}
pat : pairpat {$1}

pairpat :: {BPattern}
pairpat : eitherpat ',' eitherpat {PPair (B $1) (B $3)}
        | eitherpat {$1}

eitherpat :: {BPattern}
eitherpat : Left typ typ bottompat {PLeft $2 $3 (B $4)}
         | Right typ typ bottompat {PRight $2 $3 (B $4)}
          | bottompat {$1}

bottompat :: {BPattern}
bottompat : '_' {PBase (B Nothing)}
           | var {PBase (B (Just $1))}
           | '()' {PUnit}
           | '(' pat ')' {$2}

typ :: {Type}
typ : eithertyp '->' typ {TFun $1 $3}
    | eithertyp {$1}

eithertyp :: {Type}
eithertyp : pairtyp '+' pairtyp {TEither $1 $3}
          | pairtyp {$1}

pairtyp :: {Type}
pairtyp : bottomtyp '*' bottomtyp {TPair $1 $3}
        | bottomtyp {$1}

bottomtyp :: {Type}
bottomtyp : unit {TUnit}
          | '(' typ ')' {$2}

{
parseError :: [L.Token] -> a
parseError token = error $ "unexpected token " ++ (show token)
}
