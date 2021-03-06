{
module Lexer(
  Token(..),
  alexLex
) where
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Word as W
import qualified Codec.Binary.UTF8.String as U
}

$lilalpha = [a-z] -- reserved for expression variables
$bigalpha = [A-Z] -- reserved for type variables

tokens :-
$white+ ;
"unit" {\_ -> TUNIT}
"->" {\_ -> ARROW}
"mu" {\_ -> MU}
"." {\_ -> DOT}
"*" {\_ -> PROD}
"+" {\_ -> SUM}
"()" {\_ -> UNIT}
"fn" {\_ -> FUN}
":" {\_ -> COLON}
"=>" {\_ -> MAPSTO}
"fold" {\_ -> FOLD}
"unfold" {\_ -> UNFOLD}
"let" {\_ -> LETL}
"=" {\_ -> EQL}
"in" {\_ -> INL}
"," {\_ -> COMMA}
"fst" {\_ -> FST}
"snd" {\_ -> SND}
"Left" {\_ -> LEFTS}
"Right" {\_ -> RIGHTS}
"case" {\_ -> CASEC}
"of" {\_ -> OFC}
"|" {\_ -> MIDC}
"[" {\_ -> LBRACK}
"]" {\_ -> RBRACK}
"(" {\_ -> LPAREN}
")" {\_ -> RPAREN}
$lilalpha [$lilalpha \_ \’]* {\s -> VAR s}
$bigalpha [$lilalpha \_ \’]* {\s -> TYPEVAR s}

{
data Token =
  TUNIT -- unit type
  | ARROW -- function type arrow
  | UNIT -- unit value
  | MU -- recursive type binder
  | DOT -- recursive type syntax
  | PROD -- product type
  | SUM -- sum type
  | FUN -- fun keyword
  | COLON -- has type colon
  | MAPSTO -- => token
  | FOLD -- recursive type fold
  | UNFOLD -- recursive type unfold
  | LETL -- let
  | EQL -- =
  | INL -- in
  | COMMA -- (e1,e2)
  | FST
  | SND
  | LEFTS
  | RIGHTS
  | CASEC -- case e of e1 | e2
  | OFC
  | MIDC
  | LBRACK
  | RBRACK
  | LPAREN
  | RPAREN
  | TYPEVAR String
  | VAR String
  | EOF
  deriving (Eq,Show)

type AlexInput = (Char, [W.Word8], String)

alexGetByte :: AlexInput -> Maybe (W.Word8,AlexInput)
alexGetByte (c,(b:bs),s) = Just (b,(c,bs,s))
alexGetByte (c,[],[]) = Nothing
alexGetByte (_,[],(c:s)) =
  case U.encodeChar c of
    (b:bs) -> Just (b, (c, bs, s))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (c,_,_) = c

-- alexScanTokens :: String -> [token]
alexScanTokens str =
  go ('\n',[],str)
    where
      go inp@(_,_bs,str) =
        case alexScan inp 0 of
          AlexEOF -> []
          AlexError (c,_,_) -> error $
            "Lexical error at char: " ++ [c]
          AlexSkip inpp len -> go inpp
          AlexToken inpp len act -> act (take len str) : go inpp

alexLex = alexScanTokens
}
