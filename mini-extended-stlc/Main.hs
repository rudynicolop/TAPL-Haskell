{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Main where

import qualified AST                as A
import qualified Check              as C
import qualified Data.Map.Strict    as M
import qualified Lexer              as L
import qualified Parser             as P
import           System.Environment as OS

main = do
  args <- OS.getArgs
  case args of
    [] -> putStrLn "Failed to pass in an stlc program as an argument"
    source:_ -> do
      pgm <- readFile source
      let ast = P.parse $ L.alexLex pgm
      putStrLn $ show ast
      case C.check M.empty ast of
        Left msg           -> do putStrLn $ "Type-Error: " ++ msg
        Right (C.R t tast) -> do
          putStrLn "Successfully Type-checks! :D"
          putStrLn $ "Program type: " ++ show t
          putStrLn $ show tast
