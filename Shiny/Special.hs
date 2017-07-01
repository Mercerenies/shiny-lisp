
module Shiny.Special(argListValue, implicitValue, delimiterValue, dotDelimiterValue, userPrint) where

import Shiny.Vars
import Shiny.Symbol
import Shiny.Structure

argListValue :: Symbols Expr Expr
argListValue = getSymbolOrDefault argListName Nil

implicitValue :: Symbols Expr Expr
implicitValue = getSymbolOrDefault implicitName Nil

delimiterValue :: Symbols Expr Expr
delimiterValue = getSymbolOrDefault delimiterName (String " ")

dotDelimiterValue :: Symbols Expr Expr
dotDelimiterValue = getSymbolOrDefault dotDelimiterName (String " . ")

userPrint :: Expr -> Symbols Expr String
userPrint Nil = pure ""
userPrint (Cons x Nil) = userPrint x
userPrint (Cons x (y @ Cons {})) = do
  delim <- fromExpr <$> delimiterValue
  x' <- userPrint x
  y' <- userPrint y
  return $ x' ++ delim ++ y'
userPrint (Cons x y) = do
  delim <- fromExpr <$> dotDelimiterValue
  x' <- userPrint x
  y' <- userPrint y
  return $ x' ++ delim ++ y'
userPrint (String s) = pure s
userPrint (Atom s) = pure s
userPrint (Number n) = pure $ show n
userPrint x = pure $ printable x
