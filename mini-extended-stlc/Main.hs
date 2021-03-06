{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Main where

import qualified AST                as A
import qualified Check              as C
import qualified Data.Map.Strict    as M
import qualified Eval               as E
import qualified Lexer              as L
import qualified Parser             as P
import qualified System.Environment as OS

main = do
  args <- OS.getArgs
  case args of
    [] -> do putStrLn "Failed to pass in an stlc program as an argument"
    source:_ -> do
      pgm <- readFile source
      let ast = P.parse $ L.alexLex pgm
      putStrLn $ show ast
      case C.check M.empty ast of
        (Left tmsg)          -> do putStrLn $ "Type-Error: " ++ tmsg
        (Right (C.R t tast)) -> do
          putStrLn "Successfully Type-checks! :D"
          putStrLn $ show tast
          putStrLn $ "Program type: " ++ show t
          case E.eval M.empty tast of
            (Left emsg) -> do putStrLn $ "Evaluation-Error: " ++ emsg
            (Right v)   -> do putStrLn $ "Program evaluates to " ++ show v
