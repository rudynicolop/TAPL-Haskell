module Main where

import qualified Lexer as L
import qualified Parser as P
import AST
import qualified Eval as E
import Codec.Binary.UTF8.String (encode)

main :: IO ()
main = do
  putStrLn "Enter an arithmetic program:";
  bytes <- getLine;
  case L.evalP P.parse (encode bytes) of
    Right program -> do
      putStrLn (show program);
      putStrLn $ show $ E.eval program
    Left errormsg -> putStrLn errormsg
