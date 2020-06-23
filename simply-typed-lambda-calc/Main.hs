module Main where

import qualified AST    as A
-- import qualified Checker as C
-- import qualified Eval    as E
import qualified Lexer  as L
import qualified Parser as P

main :: IO ()
main = do
  putStrLn "Enter a Simply-Typed Lambda Calculus program!"
  p <- getLine;
  p A.|> L.alexLex A.|> P.parse A.|> show A.|> putStrLn
