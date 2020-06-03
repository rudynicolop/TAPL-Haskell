{
module Lexer (Token(..),P,evalP,lexer) where
import Control.Monad.State
import Control.Monad.Except
import Data.Word
}

tokens :-
$white+ ;
true {TRUE}
false {FALSE}
"0"	{ZERO}
succ {SUCC}
pred {PRED}
if {IF}
then {THEN}
else {ELSE}
iszero {ISZERO}
"(" {LPAREN}
")" {RPAREN}

{
data Token =
    TRUE
    | FALSE
    | ZERO
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
type AlexInput = [Word8]
-- gets the next byte to be lexed from the byte stream
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
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

evalP::P a -> AlexInput -> Either String a
evalP = evalStateT

-- maps the input to a token stream when repeatly iterated
readToken::P Token
readToken = do
    s <- get
    case alexScan s 0 of
        AlexEOF -> return EOF -- done lexing
        AlexError _ -> throwError "!Lexical error" -- screw up
        AlexSkip inp' _ -> do
            put inp'  -- put the rest of the byte stream in the state
            readToken -- recurse
        AlexToken inp' _ tk -> do
            put inp' -- put the rest of the byte stream in the state
            return tk -- ignores the () and makes tk the value

lexer::(Token -> P a)->P a
lexer cont = do
    -- passes the next token into the continuation
    token <- readToken
    cont token
}
