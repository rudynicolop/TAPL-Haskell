{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Main where

import qualified AST                as A
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
      putStrLn $ "Parsed program as: "
      putStrLn $ show ast
