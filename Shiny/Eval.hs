{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Shiny.Eval where

import Control.Monad
import Shiny.Structure
import Shiny.Symbol
import Shiny.Vars

evalSeq :: [Expr] -> Symbols Expr Expr
evalSeq [] = pure Nil
evalSeq xs = last <$> mapM evaluate xs

functionCall :: Expr -> [Expr] -> Symbols Expr Expr
functionCall (BuiltIn f) args = runFunc f args
functionCall (Special f) args = runFunc f args
functionCall f args = do
  pushStack
  bindArgs args
  ev <- evaluate f
  void popStack
  return ev

evalList :: Expr -> Expr -> Symbols Expr Expr
evalList car cdr = do
  function <- evaluate car
  xs <- exprToList' cdr
  result <- case function of
              Special {} -> functionCall function xs
              function' -> do
                            args <- mapM evaluate xs
                            functionCall function' args
  return result

evaluate :: Expr -> Symbols Expr Expr
evaluate (Cons x y) = evalList x y
evaluate (Atom x) = getSymbolDefining (Var x) Nil
evaluate x = pure x -- For strings, numbers, and Nil, they are literally themselves
