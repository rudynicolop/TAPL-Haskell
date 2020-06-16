module Lexer(lexx) where

import           AST
import qualified Data.Char as C

lexx :: String -> [Token]
lexx [] = []
lexx ('\\':stream) = LAMBDA : lexx stream
lexx ('.':stream) = DOT : lexx stream
lexx ('(':stream) = LPAREN : lexx stream
lexx (')':stream) = RPAREN : lexx stream
lexx ('s':'u':'c':'c':stream) = SUCC : lexx stream
lexx ('i':'d':stream) = ID : lexx stream
lexx ('a':'d':'d':stream) = ADD : lexx stream
lexx ('m':'u':'l':'t':stream) = MULT : lexx stream
lexx ('p':'r':'e':'d':stream) = PRED : lexx stream
lexx ('s':'u':'b':stream) = SUB : lexx stream
lexx ('t':'r':'u':'e':stream) = TRUE : lexx stream
lexx ('f':'a':'l':'s':'e':stream) = FALSE : lexx stream
lexx ('c':'o':'n':'d':stream) = COND : lexx stream
lexx ('a':'n':'d':stream) = AND : lexx stream
lexx ('o':'r':stream) = OR : lexx stream
lexx ('i':'s':'z':'e':'r':'o':stream) = ISZERO : lexx stream
lexx ('e':'q':stream) = AST.EQ : lexx stream
lexx ('Y':stream) = Y : lexx stream
lexx (c:stream)
  | C.isSpace c = lexx stream
  | C.isDigit c = spanned C.isDigit $ NAT . read
  | C.isAlpha c = spanned C.isAlpha VAR
  where
    spanned f t =
      let (x,rest) = span f (c:stream) in
      (t x) : lexx rest
