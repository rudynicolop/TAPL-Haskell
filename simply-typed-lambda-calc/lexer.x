
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

$digit = 0-9 -- digits

$alpha = [a-zA-Z] -- alphabetic characters

tokens :-
$white+ ;
$digit+ { \s -> NNUM $ read s}
nat {\_ -> NATTYPE}
bool {\_ -> BULTYPE}
true {\_ -> TRUE}
false {\_ -> FALSE}
Z	{\_ -> ZERO}
S {\_ -> SUCC}
"." {\_ -> DOT}
"->" {\_ -> ARROW}
"!" {\_ -> NOT}
"+" {\_ -> ADD}
"-" {\_ -> SUB}
"=" {\_ -> TEQ}
"<" {\_ -> TLE}
"&" {\_ -> AND}
"|" {\_ -> OR}
":" {\_ -> HASTYPE}
"\\" {\_ -> LAMBDA}
if {\_ -> IF}
then {\_ -> THEN}
else {\_ -> ELSE}
"(" {\_ -> LPAREN}
")" {\_ -> RPAREN}
$alpha [$alpha \_ \’]* { \s -> VAR s }
{

data Token =
  NNUM Integer
  | NATTYPE
  | BULTYPE
  | TRUE
  | FALSE
  | ZERO
  | SUCC
  | DOT
  | ARROW
  | NOT
  | ADD
  | MUL
  | SUB
  | TEQ
  | TLE
  | AND
  | OR
  | HASTYPE
  | LAMBDA
  | IF
  | THEN
  | ELSE
  | EOF
  | LPAREN
  | RPAREN
  | VAR String
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
          AlexError _ -> error "lexical error"
          AlexSkip inpp len -> go inpp
          AlexToken inpp len act -> act (take len str) : go inpp

alexLex = alexScanTokens
}
