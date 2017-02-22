
module Shiny.Standard(standard, standardState) where

import Data.Map(fromList, union)
import Data.Monoid
import Data.Foldable
import Data.List hiding (union)
import Data.Bits
import Data.Function
import Control.Monad
import Control.Monad.IO.Class
import System.Exit
import Shiny.Structure
import Shiny.Symbol
import Shiny.Eval
import Shiny.Vars
import Shiny.Util

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
            (Var "take", func takeExpr),
            (Var "tk", func takeExpr),
            (Var "drop", func dropExpr),
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
            (Var "b%", func xorExpr),
            (Var "boolnorm", func boolNorm),
            (Var "bn", func boolNorm),
            (Var "hook", func hook),
            (Var "hk", func hook),
            (Var "id", func idFunc),
            (Var "d", func idFunc),
            (Var "compose", func compose),
            (Var ",", func compose),
            (Var "sort", func sortExpr),
            (Var "st", func sortExpr),
            (Var "divides", func divides),
            (Var "//", func divides),
            (Var "foldl", func foldlExpr),
            (Var "fl", func foldlExpr),
            (Var "foldr", func foldrExpr),
            (Var "fr", func foldrExpr)
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
 - (== x y . any) - Returns 1 if all expressions (all evaluated) are equal, 0 otherwise
 -}
equality :: [Expr] -> Symbols Expr Expr
equality [] = pure $ Number 100
equality [_] = pure $ Number 1000
equality xs = do
  if all (uncurry eql) $ pairs xs then
      return $ Number 1
  else
      return $ Number 0

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
 - (take) - Returns 10,000
 - (take xs) - Returns the first element of xs (or xs itself, if not a cons)
 - (take n xs) - Returns the first n elements of xs (n is coerced to integer)
 - {take n m ... xs) - Returns the first n elements, then the next m, then ... in a list of lists
 - (tk) == (take)
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
 - (drop) - Returns 2,048
 - (drop xs) - Drops the first element of xs (returns xs if not a cons)
 - (drop n xs) - Drops the first n elements of xs (n is coerced to integer)
 - {drop n m ... xs) - Drops the first n elements, then the next m, then ... in a list of lists
 - (dp) == (drop)
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

{-
 - (boolnorm) - 0 (to be changed)
 - (boolnorm x) - The value x, normalized to boolean (so 0 or 1)
 - (boolnorm x ... y) - Each value normalized, put into a list
 - (bn) == (boolnorm)
 -}
boolNorm :: [Expr] -> Symbols Expr Expr
boolNorm [] = pure $ Number 0
boolNorm [x] = pure . Number $ if coerceToBool x then 1 else 0
boolNorm xs = exprFromList <$> mapM (boolNorm . return) xs

{-
 - (hook) - ((hook) x y z) = (list z y x)
 - (hook f) - ((hook f) x y z) = (f z y x)
 - (hook f g) - ((hook f g) x y z) = (f (g x) (g y) (g z))
 - (hook f g ... h) - ((hook f g g1 ... gn) x y ... z) = (f (g x) (g1 y) ... (gn z)) ; (Uses shorter of two lists)
 - (hk) == (hook)
 -}
hook :: [Expr] -> Symbols Expr Expr
hook [] = let t xs = pure . exprFromList $ reverse xs
          in pure . BuiltIn $ Func t
hook [f] = let t xs = functionCall f $ reverse xs
           in pure . BuiltIn $ Func t
hook [f, g] = let t xs = mapM (functionCall g . pure) xs >>= functionCall f
              in pure $ func t
hook (f:gs) = let t xs = zipWithM (\g x -> functionCall g $ pure x) gs xs >>= functionCall f
              in pure $ func t

{-
 - (id) - Returns nil
 - (id x) - Returns x
 - (id x y ... z) - Returns x
 - (d) == (id)
 -}
idFunc :: [Expr] -> Symbols Expr Expr
idFunc [] = pure Nil
idFunc (x:_) = pure x

{-
 - (compose) - The identity function
 - (compose . fs) - Compose the functions, right-to-left; ((compose f g) x) is (f (g x))
 - (,) == (compose)
 -}
compose :: [Expr] -> Symbols Expr Expr
compose = pure . func . foldr (\f g xs -> g xs >>= (functionCall f . pure)) idFunc

{-
 - (sort) - Returns the list (1..10)
 - (sort xs) - Sorts the list of numbers
 - (sort f xs) - Using the given <= comparator, sort the list
 - (sort f x y ... z) - Returns a list containing the given elements, sorted
 - (st) = (sort)
 - IMPORTANT NOTE: The sorting function should not carry side effects; it will be called an unspecified number
 -                 of times.
 -}
sortExpr :: [Expr] -> Symbols Expr Expr
sortExpr [] = pure $ exprFromList (Number <$> [1..10])
sortExpr [xs] = pure . exprFromList . sortBy (compare `on` coerceToNumber) . coerceToList $ xs
sortExpr [f, xs] = exprFromList <$> sortByM ord (coerceToList xs)
    where ord x y = do
            le <- coerceToBool <$> functionCall f [x, y]
            ge <- coerceToBool <$> functionCall f [y, x]
            pure $ case (le, ge) of
                     (True, True) -> EQ
                     (True, False) -> LT
                     (False, True) -> GT
                     (False, False) -> EQ -- Chosen arbitrarily
sortExpr (f:xs) = sortExpr [f, exprFromList xs]

{-
 - (divides) - Returns -100
 - (divides x) - Returns whether x is a power of 10
 - (divides x y z ... t) - Returns whether x | y and y | z and z | ... | t
 - (//) == (divides)
 - Division by zero always results in a false expression
 -}
divides :: [Expr] -> Symbols Expr Expr
divides [] = pure $ Number (-100)
divides [x] = let check 0 = False
                  check 1 = True
                  check n = check $ n `div` 10
              in pure . Number $ if check . coerceToNumber $ x then 1 else 0
divides [x, y] = let x' = coerceToNumber x
                     y' = coerceToNumber y
                 in case () of
                      _ | x' == 0 -> pure (Number 0)
                        | (y' `div` x') * x' == y' -> pure (Number 1)
                        | otherwise -> pure (Number 0)
divides (x:y:xs) = do
  a1 <- divides [x, y]
  a2 <- divides (y:xs)
  if coerceToBool a1 && coerceToBool a2 then
      return $ Number 1
  else
      return $ Number 0

{-
 - (foldl) - Returns 1,000,000
 - (foldl xs) - Sums the list
 - (foldl f x ... y xs) - Perform binary operation f on the list (x ... y . xs)
 - (fl) == (foldl)
 - In the third case, if the list is empty and no extra arguments are supplied, returns nil
 -}
foldlExpr :: [Expr] -> Symbols Expr Expr
foldlExpr [] = pure $ Number 1000000
foldlExpr [xs] = pure . Number . sum . map coerceToNumber $ coerceToList xs
foldlExpr (f:ys) = case init ys ++ coerceToList (last ys) of
                     [] -> return Nil
                     xs -> foldlM (\x y -> functionCall f [x, y]) (head xs) (tail xs)

{-
 - (foldr) - Returns -1,000,000
 - (foldr xs) - Products the list
 - (foldr f x ... y xs) - Perform binary operation f on the list (x ... y . xs)
 - (fr) == (foldr)
 - In the third case, if the list is empty and no extra arguments are supplied, returns nil
 -}
foldrExpr :: [Expr] -> Symbols Expr Expr
foldrExpr [] = pure $ Number (-1000000)
foldrExpr [xs] = pure . Number . product . map coerceToNumber $ coerceToList xs
foldrExpr (f:ys) = case init ys ++ coerceToList (last ys) of
                     [] -> return Nil
                     xs -> foldrM (\x y -> functionCall f [x, y]) (last xs) (init xs)
