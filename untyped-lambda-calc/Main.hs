module Main where
import           AST
import           Eval
import           Lexer
import           Parser



main :: IO ()
main = do
  putStrLn "Enter an Untyped Lambda Calculus program!"
  p <- getLine;
  p |> lexx |> parse |> eval |> show |> putStrLn
