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

$alpha = [a-zA-Z] -- alphabetic characters

tokens :-
$white+ ;
"unit" {\_ -> TUNIT}
"->" {\_ -> ARROW}
"*" {\_ -> PROD}
"+" {\_ -> SUM}
"_" {\_ -> WILD}
"()" {\_ -> UNIT}
"," {\_ -> COMMA}
"Left" {\_ -> LEFT}
"Right" {\_ -> RIGHT}
"||" {\_ -> POR}
"fun" {\_ -> FUN}
":" {\_ -> COLON}
"=>" {\_ -> MAPSTO}
"let" {\_ -> LET}
"=" {\_ -> LETEQ}
"in" {\_ -> IN}
"fst" {\_ -> FST}
"snd" {\_ -> SND}
"match" {\_ -> MATCH}
"with" {\_ -> WITH}
"|" {\_ -> MID}
"end" {\_ -> END}
"(" {\_ -> LPAREN}
")" {\_ -> RPAREN}
$alpha [$alpha \_ \’]* { \s -> VAR s }

{
data Token =
  TUNIT -- unit type
  | ARROW -- function type arrow
  | PROD -- product type *
  | SUM -- either type +
  | WILD -- wild pattern
  | UNIT -- unit value
  | COMMA -- comma in pair
  | LEFT -- either left
  | RIGHT -- either right
  | POR -- or pattern delimeter
  | FUN -- fun keyword
  | COLON -- has type colon
  | MAPSTO -- => token
  | LET -- let token
  | LETEQ -- = token
  | IN -- in token
  | FST -- fst
  | SND -- snd
  | MATCH -- match
  | WITH -- with
  | MID -- match-cases delimeter
  | END -- end of pattern match
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
