module Main where

import           AST
import           Codec.Binary.UTF8.String (encode)
import qualified Eval                     as E
import qualified Lexer                    as L
import qualified Parser                   as P

main :: IO ()
main = do
  putStrLn "Enter an arithmetic program:";
  bytes <- getLine;
  case L.evalP P.parse (encode bytes) of
    Right program -> do
      putStrLn (show program);
      putStrLn $ show $ E.eval program
    Left errormsg -> putStrLn errormsg
