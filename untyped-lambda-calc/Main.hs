module Main where
import           AST
import           Eval
import           Lexer
import           Parser



main :: IO ()
main = do
  putStrLn "Enter an Untyped Lambda Calculus program!";
  pgm <- getLine;
  pgm |> lexx |> parse |> show |> putStrLn
