{
module Lexer (Token(..),P,evalP,lexer) where
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Word as W
import AST
import qualified Conversion as C
import qualified Data.Char as DC
}

$digit = 0-9 -- digits

tokens :-
$white+ ;
$digit+ { \s -> NNUM $ read $ map (DC.chr . fromIntegral) s}
true {\_ -> TRUE}
false {\_ -> FALSE}
"Z"	{\_ -> ZERO}
succ {\_ -> SUCC}
pred {\_ -> PRED}
if {\_ -> IF}
then {\_ -> THEN}
else {\_ -> ELSE}
iszero {\_ -> ISZERO}
"(" {\_ -> LPAREN}
")" {\_ -> RPAREN}

{

data Token =
    TRUE
    | FALSE
    | ZERO
    | NNUM Integer
    | SUCC
    | PRED
    | IF
    | THEN
    | ELSE
    | ISZERO
    | EOF
    | LPAREN
    | RPAREN
    deriving (Eq,Show)

-- byte stream, lexer input type
type AlexInput = [W.Word8]
-- gets the next byte to be lexed from the byte stream
alexGetByte :: AlexInput -> Maybe (W.Word8,AlexInput)
alexGetByte (b:bs) = Just (b,bs)
alexGetByte []     = Nothing

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = undefined

-- a function type:
-- AlexInput -> Either String (a,AlexInput)
-- this is the type of the readToken function
-- It maps the byte stream to either a String (the error message) or
-- the current token and the rest of the stream
type P a = StateT AlexInput (Either String) a

evalP :: P a -> AlexInput -> Either String a
evalP = evalStateT

-- maps the input to a token stream when repeatly iterated
readToken :: P Token
readToken = do
  s <- get
  case alexScan s 0 of
    AlexEOF -> return EOF -- done lexing
    AlexError _ -> throwError "!Lexical error" -- screwed up
    AlexSkip inp _ -> do
      put inp  -- put the rest of the byte stream in the state
      readToken -- recurse
    AlexToken inp len tk -> do
      put inp -- put the rest of the byte stream in the state
      return $ tk (take len s)

lexer :: (Token -> P a) -> P a
lexer cont = do
  -- passes the next token into the continuation
  token <- readToken
  cont token
}
