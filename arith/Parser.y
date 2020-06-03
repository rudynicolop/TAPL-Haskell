{
module Parser(parse) where
import AST
import qualified Lexer as L
import Control.Monad.Error
}

%monad{L.P}
%lexer{L.lexer}{L.EOF}
%name parse
%tokentype{L.Token}
%error {parseError}

%token
true {L.TRUE}
false {L.FALSE}
zero {L.ZERO}
succ {L.SUCC}
pred {L.PRED}
if {L.IF}
then {L.THEN}
else {L.ELSE}
iszero {L.ISZERO}
lparen {L.LPAREN}
rparen {L.RPAREN}
%%

Term : true { TTrue }
| false {TFalse}
| zero { Zero }
| iszero Term {IsZero $2}
| succ Term { Succ $2 }
| pred Term { Pred $2 }
| if Term then Term else Term { IfThenElse $2 $4 $6 }
| lparen Term rparen { $2 }

{
parseError :: L.Token -> a
parseError _ = error "Parse error"
}
