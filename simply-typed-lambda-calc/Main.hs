module Main where

import qualified AST             as A
import qualified Checker         as C
import qualified Data.Map.Strict as M
import qualified Eval            as E
import qualified Lexer           as L
import qualified Parser          as P

main :: IO ()
main = do
  putStrLn "Enter a Simply-Typed Lambda Calculus program!"
  p <- getLine;
  let ast = p A.|> L.alexLex A.|> P.parse
  putStrLn $  show ast
  case C.check M.empty ast of
    Left msg           -> do putStrLn $ "Type-Error: " ++ msg
    Right (C.R t tast) -> do
      putStrLn "Successfully Type-checks! :D"
      putStrLn $ "Program type: " ++ show t
      putStrLn $ show tast
      putStrLn $ "Evaluates to: " ++ (show $ E.stepstar tast)
