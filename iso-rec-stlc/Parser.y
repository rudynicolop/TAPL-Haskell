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
mu {L.MU}
'.' {L.DOT}
'*' {L.PROD}
'+' {L.SUM}
'()' {L.UNIT}
fn {L.FUN}
':' {L.COLON}
'=>' {L.MAPSTO}
fold {L.FOLD}
unfold {L.UNFOLD}
let {L.LETL}
'=' {L.EQL}
in {L.INL}
',' {L.COMMA}
fst {L.FST}
snd {L.SND}
Left {L.LEFTS}
Right {L.RIGHTS}
case {L.CASEC}
of {L.OFC}
'|' {L.MIDC}
'[' {L.LBRACK}
']' {L.RBRACK}
'(' {L.LPAREN}
')' {L.RPAREN}
var {L.VAR $$}
typevar {L.TYPEVAR $$}

%%

expr :: {Expr UA UR}
expr : let var '=' funexpr in expr {ELet $2 (UA $4) (UA $6)}
     | funexpr {$1}

funexpr :: {Expr UA UR}
funexpr : fn var ':' typ '=>' funexpr {EFun $2 $4 (UA $6)}
        | caseexpr {$1}

caseexpr :: {Expr UA UR}
caseexpr : case expr of funexpr '|' funexpr {ECase (UA $2) (UA $4) (UA $6)}
         | pairexpr {$1}

pairexpr :: {Expr UA UR}
pairexpr : '(' projexpr ',' projexpr ')' {EPrd (UA $2) (UA $4)}
        | projexpr {$1}

projexpr :: {Expr UA UR}
projexpr : fst foldexpr {EFst $ UA $2}
         | snd foldexpr {ESnd $ UA $2}
         | foldexpr {$1}

foldexpr :: {Expr UA UR}
foldexpr : fold '[' typ ']' appexpr {EFold (UR $3) (UA $5)}
         | unfold '[' typ ']' foldexpr {EUnfold (UR $3) (UA $5)}
         | appexpr {$1}

appexpr :: {Expr UA UR}
appexpr : appexpr eitherexpr {EApp (UA $1) (UA $2)}
        | eitherexpr {$1}

eitherexpr :: {Expr UA UR}
eitherexpr : Left typ unitexpr {ELeft $2 $ UA $3}
           | Right typ unitexpr {ERight $2 $ UA $3}
           | unitexpr {$1}

unitexpr :: {Expr UA UR}
unitexpr : '()' {EUnit}
      | varexpr {$1}

varexpr :: {Expr UA UR}
varexpr : var {EVar (UA $1)}
     | '(' expr ')' {$2}

typ :: {Type}
typ : mu typevar '.' typ {TRec $2 $4}
    | funtyp {$1}

funtyp :: {Type}
funtyp : sumtyp '->' funtyp {TFun $1 $3}
     | sumtyp {$1}

sumtyp :: {Type}
sumtyp : prodtyp '+' prodtyp {TSum $1 $3}
        | prodtyp {$1}

prodtyp :: {Type}
prodtyp : unittyp '*' unittyp {TPrd $1 $3}
        | unittyp {$1}

unittyp :: {Type}
unittyp : unit {TUnit}
        | vartyp {$1}

vartyp :: {Type}
vartyp  : typevar {TVar $1}
        | '(' typ ')' {$2}

{
parseError :: [L.Token] -> a
parseError token = error $ "unexpected token " ++ (show token)
}
