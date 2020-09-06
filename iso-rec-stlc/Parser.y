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
'()' {L.UNIT}
fn {L.FUN}
':' {L.COLON}
'=>' {L.MAPSTO}
fold {L.FOLD}
unfold {L.UNFOLD}
'[' {L.LBRACK}
']' {L.RBRACK}
'(' {L.LPAREN}
')' {L.RPAREN}
var {L.VAR $$}
typevar {L.TYPEVAR $$}

%%

expr :: {Expr UA UR}
expr : fn var ':' typ '=>' expr {EFun $2 $4 (UA $6)}
     | foldexpr {$1}

foldexpr :: {Expr UA UR}
foldexpr : fold '[' typ ']' appexpr {EFold (UR $3) (UA $5)}
         | unfold '[' typ ']' foldexpr {EUnfold (UR $3) (UA $5)}
         | appexpr {$1}

appexpr :: {Expr UA UR}
appexpr : appexpr vexpr {EApp (UA $1) (UA $2)}
       | uexpr {$1}

uexpr :: {Expr UA UR}
uexpr : '()' {EUnit}
      | vexpr {$1}

vexpr :: {Expr UA UR}
vexpr : var {EVar (UA $1)}
     | '(' expr ')' {$2}

typ :: {Type}
typ : mu typevar '.' typ {TRec $2 $4}
    | ftyp {$1}

ftyp :: {Type}
ftyp : utyp '->' ftyp {TFun $1 $3}
     | utyp {$1}

utyp :: {Type}
utyp : unit {TUnit}
     | vtyp {$1}

vtyp :: {Type}
vtyp  : typevar {TVar $1}
      | '(' typ ')' {$2}

{
parseError :: [L.Token] -> a
parseError token = error $ "unexpected token " ++ (show token)
}
