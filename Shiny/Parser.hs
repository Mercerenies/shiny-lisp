
module Shiny.Parser(readExpr, readExpr', readSingleExpr, readSingleExpr') where

import Shiny.Structure
import Shiny.Case
import Data.Bifunctor
import Control.Monad
import Text.Parsec hiding (lower, upper)
import qualified Text.ParserCombinators.Parsec.Number as Number

type Parser a = Parsec String () a

readExpr :: String -> Either String [Expr]
readExpr = first show . readExpr'

readExpr' :: String -> Either ParseError [Expr]
readExpr' = parse (exprs <* eof) "(eval)"

readSingleExpr :: String -> Either String Expr
readSingleExpr = first show . readSingleExpr'

readSingleExpr' :: String -> Either ParseError Expr
readSingleExpr' = parse expr "(eval)"

exprs :: Parser [Expr]
exprs = concat <$> (spaces *> many (listContents <* spaces))

expr :: Parser Expr
expr = list <|> quoted <|> toExpr <$> upperChain

quoted :: Parser Expr
quoted = char '\'' *> (doQuote <$> (expr <|> lowerExpr))

doQuote :: Expr -> Expr
doQuote x = toExpr [Atom "quote", x]

doProgn :: Expr -> Expr
doProgn = Cons (Atom "progn")

stringLit :: Parser Expr
stringLit = char '\"' *> (String <$> rest) <* char '\"'
    where contents = (:) <$> single <*> rest
          single = noneOf "\\\"" <|> char '\\' *> anyChar
          rest = contents <|> pure []

numberLit :: Parser Expr
numberLit = do
  sign <- option 1 (-1 <$ char '\\')
  value <- try Number.nat
  return . Number $ sign * value

-- Note on syntax:
-- (x y z) => (x y z)
-- [x y z] => '(x y z)
-- {x y z} => (progn x y z)
list :: Parser Expr
list = regularList <|> quotedList <|> seqList
    where regularList = char '(' *> spaces *> restOfList <* char ')'
          quotedList = doQuote <$> (char '[' *> spaces *> restOfList <* char ']')
          seqList = doProgn <$> (char '{' *> spaces *> restOfList <* char '}')

restOfList :: Parser Expr
restOfList = regular <|> terminating <|> pure Nil
    where regular = prepend <$> listContents <*> (spaces *> restOfList)
          terminating = char '.' *> spaces *> (expr <|> lowerExpr) <* spaces

listContents :: Parser [Expr]
listContents = pure <$> expr <|> lowerChain

lowerChain :: Parser [Expr]
lowerChain = ((:) <$> lowerExpr <*> many upperExpr) <* optional (char '~')

upperChain :: Parser [Expr]
upperChain = ((:) <$> mandatoryUpper <*> many upperExpr) <* optional (char '~')

lowerExpr :: Parser Expr
lowerExpr = numberLit <|> stringLit <|> list <|> quoted <|> atom

upperExpr :: Parser Expr
upperExpr = numberLit <|> stringLit <|> list <|> quoted <|> mandatoryUpper

mandatoryUpper :: Parser Expr
mandatoryUpper = capsAtom <|> coloned
    where coloned = do
            void $ char ':'
            lowerExpr <|> toExpr <$> upperChain

atom :: Parser Expr
atom = Atom <$> many1 lower

capsAtom :: Parser Expr
capsAtom = (Atom . map toLowerCase) <$> ((:) <$> upper <*> many lower)

upper :: Parser Char
upper = oneOf upperCase

lower :: Parser Char
lower = noneOf $ upperCase ++ special

special :: [Char]
special = ":()\"' \t\n\r1234567890.[]{}~\\"
