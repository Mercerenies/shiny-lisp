
module Shiny.Standard(standard, standardState) where

import Data.Map(fromList, union)
import Data.Monoid
import Data.List hiding (union)
import Data.Bits
import Control.Monad
import Control.Monad.IO.Class
import System.Exit
import Shiny.Structure
import Shiny.Symbol
import Shiny.Eval
import Shiny.Vars

{-
 - NOTE: The result is UNDEFINED if a special form (like 'cond, for instance) is passed as a first-class function
 -}

standard :: SymbolTable Expr
standard = stdFuncs `union` stdValues

stdFuncs :: SymbolTable Expr
stdFuncs = fromList [
            (Var "=", func' assignment),
            (Var "qq", func' quote),
            (Var "quote", func' quote),
            (Var "progn", func progn),
            (Var "pgn", func progn),
            (Var "list", func list),
            (Var "l", func list),
            (Var "print", func puts),
            (Var "pt", func puts),
            (Var "p", func plus),
            (Var "m", func minus),
            (Var "&", func times),
            (Var "/", func divs),
            (Var "cond", func' ifStmt),
            (Var "i", func' ifStmt),
            (Var "==", func equality),
            (Var "sq", func sequenceExpr),
            (Var "tk", func takeExpr),
            (Var "dp", func dropExpr),
            (Var "apply", func applyExpr),
            (Var "ap", func applyExpr),
            (Var "map", func mapExpr),
            (Var "a", func mapExpr),
            (Var "filter", func filterExpr),
            (Var "e", func filterExpr),
            (Var "lessthan", func $ orderingOp (<) coerceToNumber (Number 5) (Number (-5))),
            (Var "c,", func $ orderingOp (<) coerceToNumber (Number 5) (Number (-5))),
            (Var "lesseq", func $ orderingOp (<=) coerceToNumber (Number 5) (Number (-5))),
            (Var "=,", func $ orderingOp (<=) coerceToNumber (Number 5) (Number (-5))),
            (Var "greaterthan", func $ orderingOp (>) coerceToNumber (Number 15) (Number 25)),
            (Var "c;", func $ orderingOp (>) coerceToNumber (Number 15) (Number 25)),
            (Var "greatereq", func $ orderingOp (>=) coerceToNumber (Number 15) (Number 25)),
            (Var "=;", func $ orderingOp (>=) coerceToNumber (Number 15) (Number 25)),
            (Var "slessthan", func $ orderingOp (<) coerceToString (Number 5) (Number (-5))),
            (Var ",c", func $ orderingOp (<) coerceToString (Number 5) (Number (-5))),
            (Var "slesseq", func $ orderingOp (<=) coerceToString (Number 5) (Number (-5))),
            (Var ",=", func $ orderingOp (<=) coerceToString (Number 5) (Number (-5))),
            (Var "sgreaterthan", func $ orderingOp (>) coerceToString (Number 15) (Number 25)),
            (Var ";c", func $ orderingOp (>) coerceToString (Number 15) (Number 25)),
            (Var "sgreatereq", func $ orderingOp (>=) coerceToString (Number 15) (Number 25)),
            (Var ";=", func $ orderingOp (>=) coerceToString (Number 15) (Number 25)),
            (Var "range", func rangeExpr),
            (Var "rg", func rangeExpr),
            (Var "quit", func quitProg),
            (Var "cons", func consExpr),
            (Var "c", func consExpr),
            (Var "bitand", func andExpr),
            (Var "b&", func andExpr),
            (Var "bitor", func orExpr),
            (Var "b\\", func orExpr),
            (Var "bitxor", func xorExpr),
            (Var "b%", func xorExpr)
           ]

stdValues :: SymbolTable Expr
stdValues = fromList [
             (Var "o", Number 1),
             (Var "t", Number 10),
             (Var "n", Nil)
            ]

standardState :: [SymbolTable Expr]
standardState = [standard]

--

{-
 - (=) No-op (might change in the future)
 - (= x) Set the variable x equal to nil; returns nil (might be changed to set some preset (#) to x)
 - (= x y) Set x equal to y (y is evaluated); returns y
 - (= x1 .. xn y) Set x1, x2, ... xn equal to y (y is evaluated once); returns y
 - In any case, any variable xi's which are not atoms are ignored
 -}
assignment :: [Expr] -> Symbols Expr Expr
assignment [] = pure Nil
assignment [x] = assignment [x, Nil]
assignment xs = do
  let y = last xs
      xs' = toVars $ init xs
  y' <- evaluate y
  forM_ xs' $ \x1 -> setOrDefSymbol x1 y'
  pure y'

{-
 - (quote) - Return nil
 - (quote x) - Return the term x without evaluating it
 - (quote x ...) - Return x and ignore the other arguments
 - In any case, (qq) may be used in place of (quote)
 -}
quote :: [Expr] -> Symbols Expr Expr
quote [] = pure Nil
quote (x:_) = pure x

{-
 - (progn . any) - Evaluates all of the expressions given in order
 - (pgn) is an abbreviation for (progn)
 -}
progn :: [Expr] -> Symbols Expr Expr
progn [] = pure Nil
progn xs = pure $ last xs

{-
 - (list . any) - Produces a list containing the evaluated elements
 - (l) is an abbreviation for (list)
 -}
list :: [Expr] -> Symbols Expr Expr
list = pure . exprFromList

{-
 - (print . any) - Prints each element (evaluated), on its own line; returns Nil
 - (print) == (pt)
 -}
puts :: [Expr] -> Symbols Expr Expr
puts xs = do
  forM_ xs $ \x -> liftIO . putStrLn $ printable x
  return Nil

{-
 - (p . any) - Adds the elements
 -}
plus :: [Expr] -> Symbols Expr Expr
plus xs = do
  return . Number . getSum $ foldMap (Sum . coerceToNumber) xs

{-
 - (m) - Negative one
 - (m x) - Negate value
 - (m x y . any) - Subtract latter elements from the first
 -}
minus :: [Expr] -> Symbols Expr Expr
minus [] = pure $ Number (-1)
minus [x] = pure $ Number (- coerceToNumber x)
minus (x:xs) = do
  return . Number $ coerceToNumber x - (getSum $ foldMap (Sum . coerceToNumber) xs)

{-
 - (& . any) - Multiples the elements (note: & is lowercase of *)
 -}
times :: [Expr] -> Symbols Expr Expr
times xs = do
  return . Number . getProduct $ foldMap (Product . coerceToNumber) xs

{-
 - (/) - Ten (may become 1/2 in the future)
 - (/ x) - Return 0 (to be reciprocal in the future)
 - (/ x y . any) - Divide latter elements from the first
 - Division by zero yields the divisor
 -}
divs :: [Expr] -> Symbols Expr Expr
divs [] = pure $ Number 10
divs [_] = pure $ Number 0
divs (x:xs) = do
  let pr = (getProduct $ foldMap (Product . coerceToNumber) xs)
  if pr == 0 then
      return . Number $ coerceToNumber x
  else
      return . Number $ coerceToNumber x `div` pr

{-
 - (cond) - No-op; return nil
 - (cond . even) - Even number of args, every other arg is a condition, execute first true one
 - (cond . odd) - Odd number of arguments, last arg is "else"-clause
 - (i) is an abbreviation for (cond)
 -}
ifStmt :: [Expr] -> Symbols Expr Expr
ifStmt [] = pure Nil
ifStmt [x] = evaluate x
ifStmt (x:y:xs) = do
  x' <- evaluate x
  if coerceToBool x' then
      evaluate y
  else
      ifStmt xs

{-
 - (==) - Returns 100
 - (== x) - Returns 1000 (without evaluating x)
 - (== x y . any) - Returns 1 if all expressions (all evaluated) are equal, Nil otherwise
 -}
equality :: [Expr] -> Symbols Expr Expr
equality [] = pure $ Number 100
equality [_] = pure $ Number 1000
equality xs = do
  if all (uncurry eql) $ pairs xs then
      return $ Number 1
  else
      return Nil

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (x:y:ys) = (x, y) : pairs (y:ys)

sequential :: [Expr] -> Func
sequential xs = Func helper
    where helper [] = helper [Number 0]
          helper (n:_) = do
            n' <- coerceToNumber <$> evaluate n
            pure $ xs `genericIndex` n'

{-
 - (sq) - Returns (sq 0)
 - (sq 0) - Returns the fibonacci sequence (0 1 1 2 3 ...)
 - (sq 1) - Returns the 2^n sequence (1 2 4 8 16 32 ...)
 - (sq 2) - Returns the identity sequence (0 1 2 3 4 5 ...)
 - (sq n) - Returns a 1-ary function which returns nil
 - (sq x . y) - Ignores anything past the first argument
 - For a returned function (sqf)
 -   (sqf) - Returns (sqf 0)
 -   (sqf n . y) - Evaluates n, gets nth element of sequence, ignores y
 -}
sequenceExpr :: [Expr] -> Symbols Expr Expr
sequenceExpr [] = sequenceExpr [Number 0]
sequenceExpr (n:_) = do
  let fibo x y = Number x : fibo y (x + y)
      twon = map Number $ iterate (2 *) 1
      base = map Number $ [0..]
      nada = repeat Nil
  let n' = coerceToNumber n
  case n' of
    0 -> pure . BuiltIn . sequential $ fibo 0 1
    1 -> pure . BuiltIn . sequential $ twon
    2 -> pure . BuiltIn . sequential $ base
    _ -> pure . BuiltIn . sequential $ nada

consTake :: Integer -> Expr -> Expr
consTake 0 _ = Nil
consTake n (Cons x y) = Cons x $ consTake (n - 1) y
consTake _ x = x

consDrop :: Integer -> Expr -> Expr
consDrop 0 x = x
consDrop n (Cons _ y) = consDrop (n - 1) y
consDrop _ x = x

listCar :: Expr -> Expr
listCar (Cons x _) = x
listCar z = z

listCdr :: Expr -> Expr
listCdr (Cons _ x) = x
listCdr z = z

{-
 - (tk) - Returns 10,000
 - (tk xs) - Returns the first element of xs (or xs itself, if not a cons)
 - (tk n xs) - Returns the first n elements of xs (n is coerced to integer)
 - {tk n m ... xs) - Returns the first n elements, then the next m, then ... in a list of lists
 - Example of that last one: (tk 2 3 2 '(1 2 3 4 5 6 7 8 9)) => '((1 2) (3 4 5) (6 7))
 -}
takeExpr :: [Expr] -> Symbols Expr Expr
takeExpr [] = pure $ Number 10000
takeExpr [x] = listCar <$> evaluate x
takeExpr xs = do
  let lst = last xs
      nums = coerceToNumber <$> init xs
  return . exprFromList . snd $ mapAccumL (\acc n -> (consDrop n acc, consTake n acc)) lst nums

{-
 - (dp) - Returns 2,048
 - (dp xs) - Drops the first element of xs (returns xs if not a cons)
 - (dp n xs) - Drops the first n elements of xs (n is coerced to integer)
 - {dp n m ... xs) - Drops the first n elements, then the next m, then ... in a list of lists
 - Example of that last one: (dp 2 3 2 '(1 2 3 4)) => '((3 4) (2 3 4) (3 4))
 -}
dropExpr :: [Expr] -> Symbols Expr Expr
dropExpr [] = pure $ Number 2048
dropExpr [x] = listCdr <$> evaluate x
dropExpr xs = do
  let lst = last xs
      nums = coerceToNumber <$> init xs
  return . exprFromList $ map (flip consDrop lst) nums

{-
 - (apply) - Returns the lowercase alphabet as a string (to be changed)
 - (apply f) - Applies the function to itself
 - (apply f x y ... xs) - Applies the function to the arglist (all evaluated)
 - (ap) is an abbreviation for (apply)
 -}
applyExpr :: [Expr] -> Symbols Expr Expr
applyExpr [] = pure $ String ['a'..'z']
applyExpr [f] = functionCall f [f]
applyExpr (f:xs) = do
  let start = init xs
  end <- exprToList' $ last xs
  functionCall f $ start ++ end

{-
 - (map) - Returns the uppercase alphabet as a string
 - (map xs) - Reverse the list xs (coercing as necessary)
 - (map f . xss) - Map the function over the lists, zipping them together and taking the length of the shortest
 - (a) == (map)
 -}
mapExpr :: [Expr] -> Symbols Expr Expr
mapExpr [] = pure $ String ['A'..'Z']
mapExpr [xs] = pure . exprFromList . reverse . coerceToList $ xs
mapExpr (f:xss) = exprFromList <$> helper (map coerceToList xss)
    where helper yss
              | any null yss = pure []
              | otherwise = do
            x1 <- functionCall f (map head yss)
            x2 <- helper (map tail yss)
            return $ x1 : x2

{-
 - (filter) - Returns the lowercase alphabet as a string
 - (filter xs) - Removes all falsy elements from the list
 - (filter f xs) - Removes all elements failing the predicate
 - (filter f xs . y) - Ignores y
 - (e) == (filter)
 -}
filterExpr :: [Expr] -> Symbols Expr Expr
filterExpr [] = pure $ String ['a'..'z']
filterExpr [xs] = pure . exprFromList . filter coerceToBool . coerceToList $ xs
filterExpr (f:xs:_) = do
  let xs' = coerceToList xs
  xs'' <- filterM (\x -> coerceToBool <$> functionCall f [x]) xs'
  return $ exprFromList xs''

{-
 - All of the comparisons work the same.
 - (cmp) - Returns 15 if the comparison is GT or GE, 5 if LT or LE
 - (cmp x) - Returns 25 if the comparison is GT or GE, -5 if LT or LE
 - (cmp x ... y) - Returns 1 if the ordering is correct, 0 otherwise
 -
 - Numeric:
 - * lessthan / c,
 - * lesseq / =,
 - * greaterthan / c;
 - * greatereq / =;
 - String:
 - * slessthan / ,c
 - * slesseq / ,=
 - * sgreaterthan / ;c
 - * sgreatereq / ;=
 -}

assertOrdering :: (a -> a -> Bool) -> (Expr -> a) -> [Expr] -> Bool
assertOrdering ord f xs = all (uncurry ord) . pairs $ map f xs

orderingOp :: (a -> a -> Bool) -> (Expr -> a) -> Expr -> Expr -> ([Expr] -> Symbols Expr Expr)
orderingOp _   _ a _ []  = pure a
orderingOp _   _ _ b [_] = pure b
orderingOp ord f _ _ xs  = pure $ if assertOrdering ord f xs then Number 1 else Number 0

{-
 - (range) - Returns 1337
 - (range n) - Returns (rg 0 n)
 - (range n1 ... nm) - Returns n1..n2..n3.. ... nm
 - (rg) == (range)
 - Example: (rg 5 7 3) => '(5 6 7 6 5 4 3)
 -}
rangeExpr :: [Expr] -> Symbols Expr Expr
rangeExpr [] = pure $ Number 1337
rangeExpr [x] = rangeExpr [Number 0, x]
rangeExpr xs = pure . exprFromList . map Number . rangeHelper $ map coerceToNumber xs
    where rangeHelper [] = []
          rangeHelper [x] = [x]
          rangeHelper (x:y:ys) = case x `compare` y of
                                   EQ -> rangeHelper (y : ys)
                                   LT -> x : rangeHelper ((x + 1) : y : ys)
                                   GT -> x : rangeHelper ((x - 1) : y : ys)

{-
 - (quit . xs) - Abort the program immediately
 -}
quitProg :: [Expr] -> Symbols Expr Expr
quitProg _ = liftIO exitSuccess

{-
 - (cons) - (() . ())
 - (cons x) = x
 - (cons x y ... z) = (x y ... . z)
 - (c) == (cons)
 -}
consExpr :: [Expr] -> Symbols Expr Expr
consExpr [] = pure $ Cons Nil Nil
consExpr [x] = pure x
consExpr (x:xs) = Cons x <$> consExpr xs

{-
 - (bitand) - All one-bits
 - (bitand x ...) - Bitwise and together
 - (b&) == (bitand)
 -}
andExpr :: [Expr] -> Symbols Expr Expr
andExpr = pure . Number . foldr (.&.) (complement zeroBits) . map coerceToNumber

{-
 - (bitor) - All zero-bits
 - (bitor x ...) - Bitwise or together
 - (b\) = (bitor)
 -}
orExpr :: [Expr] -> Symbols Expr Expr
orExpr = pure . Number . foldr (.|.) zeroBits . map coerceToNumber
{-
 - (bitxor) - 256
 - (bitxor x) - Complement
 - (bitxor x ...) - Bitwise xor together
 - (b%) = (bitxor)
 -}
xorExpr :: [Expr] -> Symbols Expr Expr
xorExpr [] = pure $ Number 256
xorExpr [x] = pure . Number . complement $ coerceToNumber x
xorExpr xs = pure . Number . foldr1 xor . map coerceToNumber $ xs
