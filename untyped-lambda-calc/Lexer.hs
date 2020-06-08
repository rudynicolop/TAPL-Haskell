module Lexer(lexx) where

import           AST
import qualified Data.Char as C

lexx :: String -> [Token]
lexx [] = []
lexx ('\\':stream) = LAMBDA : lexx stream
lexx ('.':stream) = DOT : lexx stream
lexx ('(':stream) = LPAREN : lexx stream
lexx (')':stream) = RPAREN : lexx stream
lexx (c:stream)
  | C.isSpace c = lexx stream
  | C.isDigit c = lexx stream
  | C.isAlpha c =
  let (x,rest) = span C.isAlpha (c:stream) in
  (VAR x) : lexx rest
