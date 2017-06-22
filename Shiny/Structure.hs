{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Shiny.Structure(Func(..), Expr(..),
                       toVar, toVars, printable,
                       true, false,
                       func, func', exprToList, exprToList', prepend,
                       FromExpr(..), ToExpr(..), expressed, expressedM,
                       eql, bindArgs, reBindArgs, reInterpolate) where

import Shiny.Symbol
import Shiny.Vars
import Control.Monad
import Data.Maybe
import Data.Char(isDigit, ord)

newtype Func = Func { runFunc :: [Expr] -> Symbols Expr Expr }

data Expr
    = Nil
    | Cons Expr Expr
    | Atom String
    | String String
    | Number Integer
    | Regex String
    | BuiltIn Func
    | Special Func
      deriving (Show, Eq)

instance Eq Func where
    Func _ == Func _ = True

instance Show Func where
    show (Func _) = "Func"

toVar :: Expr -> Maybe Var
toVar (Atom x) = Just $ Var x
toVar _ = Nothing

toVars :: [Expr] -> [Var]
toVars = catMaybes . map toVar

-- TODO #<BuiltIn> prints even if its a higher-order function outputted from (compose), etc.

-- Converts to a user-friendly string, while the Show instance converts to a Haskell-friendly string
printable :: Expr -> String
printable Nil = "()"
printable (Cons x y) = "(" ++ printCons x y ++ ")"
printable (Atom s) = s
printable (String s) = "\"" ++ concatMap escaped s ++ "\""
    where escaped '"' = "\\\""
          escaped '\\' = "\\\\"
          escaped x = return x
printable (Number x) = let sign = if x < 0 then "\\" else ""
                       in sign ++ show (abs x)
printable (Regex s) = "[[" ++ s ++ "]]"
printable (BuiltIn _) = "#<BuiltIn>"
printable (Special _) = "#<Special>"

true :: Expr
true = Number 1

false :: Expr
false = Number 0

printCons :: Expr -> Expr -> String
printCons x (Cons y z) = printable x ++ " " ++ printCons y z
printCons x Nil        = printable x
printCons x x1         = printable x ++ " . " ++ printable x1

func :: ([Expr] -> Symbols Expr Expr) -> Expr
func = BuiltIn . Func

func' :: ([Expr] -> Symbols Expr Expr) -> Expr
func' = Special . Func

exprToList :: Expr -> Maybe [Expr]
exprToList Nil = Just []
exprToList (Cons car cdr) = (car :) <$> exprToList cdr
exprToList _ = Nothing

exprToList' :: Expr -> Symbols e [Expr]
exprToList' e = case exprToList e of
                  Nothing -> throwS "improper list cannot be converted to arglist"
                  Just x -> pure x

exprFromList :: [Expr] -> Expr
exprFromList [] = Nil
exprFromList (x:xs) = Cons x $ exprFromList xs

exprFromBool :: Bool -> Expr
exprFromBool False = false
exprFromBool True = true

prepend :: [Expr] -> Expr -> Expr
prepend []     y = y
prepend (x:xs) y = Cons x $ prepend xs y

coerceToNumber :: Expr -> Integer
coerceToNumber Nil = 0
coerceToNumber (Cons _ y) = 1 + coerceToNumber y
coerceToNumber (Atom s) = coerceToNumber (String s)
coerceToNumber (String s) = maybe 0 fst . listToMaybe $ reads s
coerceToNumber (Number x) = x
coerceToNumber (Regex s) = coerceToNumber (String s)
coerceToNumber (BuiltIn _) = 0
coerceToNumber (Special _) = 0

coerceToString :: Expr -> String
coerceToString Nil = ""
coerceToString (Cons x y) = coerceToString x ++ coerceToString y
coerceToString (Atom s) = s
coerceToString (String s) = s
coerceToString (Number x) = show x
coerceToString (Regex s) = s
coerceToString (BuiltIn _) = "function"
coerceToString (Special _) = "function"

coerceToBool :: Expr -> Bool
coerceToBool Nil = False
coerceToBool (Cons {}) = True
coerceToBool (Atom {}) = True
coerceToBool (String s) = s /= ""
coerceToBool (Number x) = x /= 0
coerceToBool (Regex s) = s /= ""
coerceToBool (BuiltIn {}) = True
coerceToBool (Special {}) = True

coerceToList :: Expr -> [Expr]
coerceToList Nil = []
coerceToList (Cons x y) = x : coerceToList y
coerceToList z = [z]

class FromExpr a where
    fromExpr :: Expr -> a

class ToExpr a where
    toExpr :: a -> Expr

instance FromExpr Expr where
    fromExpr = id

instance FromExpr Integer where
    fromExpr = coerceToNumber

instance FromExpr Int where
    fromExpr = fromInteger . coerceToNumber

instance FromExpr String where
    fromExpr = coerceToString

instance FromExpr Bool where
    fromExpr = coerceToBool

instance FromExpr [Expr] where
    fromExpr = coerceToList

instance ToExpr Expr where
    toExpr = id

instance ToExpr Integer where
    toExpr = Number

instance ToExpr Int where
    toExpr = Number . toInteger

instance ToExpr String where
    toExpr = String

instance ToExpr Bool where
    toExpr = exprFromBool

instance ToExpr [Expr] where
    toExpr = exprFromList

expressed :: (FromExpr a, ToExpr b) => (a -> b) -> Expr -> Expr
expressed f = toExpr . f . fromExpr

expressedM :: (FromExpr a, ToExpr b, Functor f) => (a -> f b) -> Expr -> f Expr
expressedM f = fmap toExpr . f . fromExpr

eql :: Expr -> Expr -> Bool
eql Nil Nil = True
eql (Cons x y) (Cons x' y') = x `eql` x' && y `eql` y'
eql (Atom s) (Atom s') = s == s'
eql (String s) (String s') = s == s'
eql (Number x) (Number x') = x == x'
eql (Regex s) s' = eql (String s) s'
eql s (Regex s') = eql s (Regex s')
eql _ _ = False

bindArgs :: [Expr] -> Symbols Expr ()
bindArgs xs = do
  forM_ (argumentBindings xs) $ \(v, e) -> do
                defSymbol v e
  defSymbol argListName $ exprFromList xs

reBindArgs :: [Expr] -> Symbols Expr ()
reBindArgs xs = do
  forM_ (reArgumentBindings xs) $ \(v, e) -> do
                defSymbol v e
  defSymbol reArgListName $ exprFromList xs

reInterpolate :: String -> [String] -> String -> String
reInterpolate match sub str0 = helper str0
    where helper [] = []
          helper ('\\':m:str) =
              case m of
                '&' -> match ++ helper str
                n | isDigit n -> let n' = (ord n - ord '1') `mod` length sub
                                     sub' = sub !! n'
                                 in sub' ++ helper str
                  | otherwise -> '\\' : n : helper str
          helper (s:str) = s : helper str
