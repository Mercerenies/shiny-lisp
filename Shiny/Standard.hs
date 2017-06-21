
module Shiny.Standard(standard, standardState) where

import Data.Map(fromList, union)
import Data.Monoid
import Data.Foldable
import Data.List hiding (union)
import Data.Bits
import Data.Char
import Data.Function
import Control.Monad
import Control.Monad.IO.Class
import System.Exit
import System.IO
import System.Environment
import Shiny.Structure
import Shiny.Parser
import Shiny.Symbol
import Shiny.Eval
import Shiny.Vars
import Shiny.Special
import Shiny.Util
import Shiny.Greek
import qualified Shiny.Case as ShinyCase

{-
 - NOTE: The result is UNDEFINED if a special form (like 'cond, for instance) is passed as a first-class function
 -}

-- ///// TODO Commands to reboot the REPL (for convenience) and to undefine variables (also, wiki)
-- ///// What about a stringbuilder interface? Or a generalized way of having interfaces?
--       Sort of a global register system for each interface to communicate without using explicit
--       variable names.

standard :: SymbolTable Expr
standard = stdFuncs `union` stdValues

stdFuncs :: SymbolTable Expr
stdFuncs = fromList [
            (Var "=", func' assignment),
            (Var "q", func' quote),
            (Var "quote", func' quote),
            (Var "progn", func progn),
            (Var "pgn", func progn),
            (Var "list", func list),
            (Var "l", func list),
            (Var "puts", func puts),
            (Var "pu", func puts),
            (Var "p", func plus),
            (Var "m", func minus),
            (Var "&", func times),
            (Var "/", func divs),
            (Var "cond", func' ifStmt),
            (Var "i", func' ifStmt),
            (Var "==", func equality),
            (Var "sequence", func sequenceExpr),
            (Var "sq", func sequenceExpr),
            (Var "take", func takeExpr),
            (Var "tk", func takeExpr),
            (Var "car", func takeExpr),
            (Var "drop", func dropExpr),
            (Var "dp", func dropExpr),
            (Var "cdr", func dropExpr),
            (Var "apply", func applyExpr),
            (Var "ap", func applyExpr),
            (Var "map", func mapExpr),
            (Var "a", func mapExpr),
            (Var "filter", func filterExpr),
            (Var "e", func filterExpr),
            (Var "less-than", func $ orderingOp (<) (fromExpr :: Expr -> Integer)),
            (Var "c,", func $ orderingOp (<) (fromExpr :: Expr -> Integer)),
            (Var "less-eq", func $ orderingOp (<=) (fromExpr :: Expr -> Integer)),
            (Var "=,", func $ orderingOp (<=) (fromExpr :: Expr -> Integer)),
            (Var "greater-than", func $ orderingOp (>) (fromExpr :: Expr -> Integer)),
            (Var "c;", func $ orderingOp (>) (fromExpr :: Expr -> Integer)),
            (Var "greater-eq", func $ orderingOp (>=) (fromExpr :: Expr -> Integer)),
            (Var "=;", func $ orderingOp (>=) (fromExpr :: Expr -> Integer)),
            (Var "str-less-than", func $ orderingOp (<) (fromExpr :: Expr -> Integer)),
            (Var ",c", func $ orderingOp (<) (fromExpr :: Expr -> Integer)),
            (Var "str-less-eq", func $ orderingOp (<=) (fromExpr :: Expr -> Integer)),
            (Var ",=", func $ orderingOp (<=) (fromExpr :: Expr -> Integer)),
            (Var "str-greater-than", func $ orderingOp (>) (fromExpr :: Expr -> Integer)),
            (Var ";c", func $ orderingOp (>) (fromExpr :: Expr -> Integer)),
            (Var "str-greater-eq", func $ orderingOp (>=) (fromExpr :: Expr -> Integer)),
            (Var ";=", func $ orderingOp (>=) (fromExpr :: Expr -> Integer)),
            (Var "range", func rangeExpr),
            (Var "rg", func rangeExpr),
            (Var "quit", func quitProg),
            (Var "cons", func consExpr),
            (Var "c", func consExpr),
            (Var "bitand", func andExpr),
            (Var "b&", func andExpr),
            (Var "bitor", func orExpr),
            (Var "b;", func orExpr),
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
            (Var "fr", func foldrExpr),
            (Var "join", func joinExpr),
            (Var "jn", func joinExpr),
            (Var "mod", func modExpr),
            (Var "md", func modExpr),
            (Var "even", func evenExpr),
            (Var "ev", func evenExpr),
            (Var "odd", func oddExpr),
            (Var "od", func oddExpr),
            (Var "strings", func stringConcat),
            (Var "s", func stringConcat),
            (Var "prime", func primeExpr),
            (Var "pm", func primeExpr),
            (Var "up", func powerExpr),
            (Var "mo", func minusOne),
            (Var "po", func plusOne),
            (Var "mt", func minusTwo),
            (Var "pt", func plusTwo),
            (Var "split", func splitExpr),
            (Var "sp", func splitExpr),
            (Var "inter", func interExpr),
            (Var "ps", func interExpr),
            (Var "insert", func insertExpr),
            (Var "it", func insertExpr),
            (Var "remove", func removeExpr),
            (Var "rm", func removeExpr),
            (Var "nth", func nthExpr),
            (Var "nh", func nthExpr),
            (Var "read", func readStmt),
            (Var "rd", func readStmt),
            (Var "eval", func evalStmt),
            (Var "el", func evalStmt),
            (Var "two-curry", func twoCurry),
            (Var "tc", func twoCurry),
            (Var "seq-while", func seqWhile),
            (Var "sw", func seqWhile),
            (Var "print-greek", func printGreek),
            (Var "pgk", func printGreek),
            (Var "each-char", func eachChar),
            (Var "ec", func eachChar),
            (Var "string-replace", func stringRepl),
            (Var "sr", func stringRepl),
            (Var "uc", func $ caseOp toUpper),
            (Var "ucx", func $ caseOp ShinyCase.toUpperCase),
            (Var "lc", func $ caseOp toLower),
            (Var "lcx", func $ caseOp ShinyCase.toLowerCase),
            (Var "sum-digits", func sumDigits),
            (Var "sd", func sumDigits),
            (Var "gets", func gets),
            (Var "ge", func gets),
            (Var "-", func' interaction),
            (Var "print", func putsPrint),
            (Var "pn", func putsPrint),
            (Var "define", func' defineVar),
            (Var ",-", func' defineVar),
            (Var "undefine", func' undefineVar),
            (Var "-,", func' undefineVar),
            (Var "argv", func getArgv),
            (Var "av", func getArgv)
           ]

stdValues :: SymbolTable Expr
stdValues = fromList [
             (Var "o", Number 1),
             (Var "w", Number 2),
             (Var "t", Number 10),
             (Var "n", Nil),
             (delimiterName, String " "),
             (dotDelimiterName, String " . ")
            ]

standardState :: [SymbolTable Expr]
standardState = [standard]

--

{-
 - (=) No-op (TODO Something?)
 - (= x) Set the special variable % to the value x; returns x
 - (= x y) Set x equal to y (y is evaluated); returns y
 - (= x1 .. xn y) Set x1, x2, ... xn equal to y (y is evaluated once); returns y
 - In any case, any variable xi's which are not atoms are ignored
 -}
assignment :: [Expr] -> Symbols Expr Expr
assignment [] = pure Nil
assignment [x] = assignment [Atom "%", x]
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
 - In any case, (q) may be used in place of (quote)
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
list = pure . toExpr

{-
 - (puts) - Prints %
 - (puts . any) - Prints each element (evaluated), on its own line; returns Nil
 - (puts) == (pu)
 -}
puts :: [Expr] -> Symbols Expr Expr
puts [] = do
  value <- implicitValue
  userPrint value >>= liftIO . putStrLn
  return Nil
puts xs = do
  forM_ xs $ userPrint >=> liftIO . putStrLn
  return Nil

{-
 - (p . any) - Adds the elements
 -}
plus :: [Expr] -> Symbols Expr Expr
plus xs = do
  return . Number . getSum $ foldMap (Sum . fromExpr) xs

{-
 - (m) - Negative one
 - (m x) - Negate value
 - (m x y . any) - Subtract latter elements from the first
 -}
minus :: [Expr] -> Symbols Expr Expr
minus [] = pure $ Number (-1)
minus [x] = pure $ Number (- fromExpr x)
minus (x:xs) = do
  return . Number $ fromExpr x - (getSum $ foldMap (Sum . fromExpr) xs)

{-
 - (& . any) - Multiples the elements (note: & is lowercase of *)
 -}
times :: [Expr] -> Symbols Expr Expr
times xs = do
  return . Number . getProduct $ foldMap (Product . fromExpr) xs

{-
 - (/) - 10 (TODO may become 1/2 in the future)
 - (/ x) - Return 0 (TODO to be reciprocal in the future)
 - (/ x y . any) - Divide latter elements from the first
 - Division by zero yields the dividend
 -}
divs :: [Expr] -> Symbols Expr Expr
divs [] = pure $ Number 10
divs [_] = pure $ Number 0
divs (x:xs) = do
  let pr = (getProduct $ foldMap (Product . fromExpr) xs)
  if pr == 0 then
      return . Number $ fromExpr x
  else
      return . Number $ fromExpr x `div` pr

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
  if fromExpr x' then
      evaluate y
  else
      ifStmt xs

{-
 - (==) - Returns 100
 - (== x) - Returns whether % equals x
 - (== x y . any) - Returns true iff all expressions (all evaluated) are equal
 -}
equality :: [Expr] -> Symbols Expr Expr
equality [] = pure $ Number 100
equality [x] = implicitValue >>= \y -> equality [x, y]
equality xs = return . toExpr . all (uncurry eql) $ pairs xs

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (x:y:ys) = (x, y) : pairs (y:ys)

sequential :: [Expr] -> Func
sequential xs = Func helper
    where helper :: [Expr] -> Symbols Expr Expr
          helper [] = helper [Number 0]
          helper (n:_) = let n' = fromExpr n :: Integer
                         in pure $ xs `genericIndex` n'

sequential' :: [Expr] -> Func
sequential' xs = Func helper
    where helper :: [Expr] -> Symbols Expr Expr
          helper [] = helper [Number 1]
          helper (n:_) = let n' = fromExpr n :: Integer
                         in pure . toExpr $ genericTake n' xs

{-
 - (sq) - Returns (sq 0)
 - (sq 0) - Returns the fibonacci sequence (0 1 1 2 3 ...)
 - (sq 1) - Returns the 2^n sequence (1 2 4 8 16 32 ...)
 - (sq 2) - Returns the identity sequence (0 1 2 3 4 5 ...)
 - (sq 3) - Returns the primes (2 3 5 7 11 ...)
 - (sq n) - Returns a 1-ary function which returns nil
 - (sq x . y) - Behaves like (sq x) but returns a sequence of the form (sqg)
 - For a returned function (sqf)
 -   (sqf) - Returns (sqf 0)
 -   (sqf n . y) - Gets nth element of sequence, ignores y
 - For a returned function (sqg)
 -   (sqg) - Returns (sqg 1)
 -   (sqg n . y) - Returns the first n elements of the sequence
 - (sequence) == (sq)
 -}
sequenceExpr :: [Expr] -> Symbols Expr Expr
sequenceExpr [] = sequenceExpr [Number 0]
sequenceExpr (n:ss) = do
  let fibo x y = Number x : fibo y (x + y)
      twon = map Number $ iterate (2 *) 1
      base = map Number $ [0..]
      primes = map Number . filter isPrime $ [2..]
      nada = repeat Nil
  let n' = fromExpr n :: Integer
      f = pure . BuiltIn . if null ss then sequential else sequential'
  case n' of
    0 -> f $ fibo 0 1
    1 -> f $ twon
    2 -> f $ base
    3 -> f $ primes
    _ -> f $ nada

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
 - (tk) == (take) == (car)
 - Example of that last one: (tk 2 3 2 '(1 2 3 4 5 6 7 8 9)) => '((1 2) (3 4 5) (6 7))
 -}
takeExpr :: [Expr] -> Symbols Expr Expr
takeExpr [] = pure $ Number 10000
takeExpr [x] = pure $ listCar x
takeExpr [n, x] = pure . toExpr $ consTake (fromExpr n) x
takeExpr xs = do
  let lst = last xs
      nums = fromExpr <$> init xs
  return . toExpr . snd $ mapAccumL (\acc n -> (consDrop n acc, consTake n acc)) lst nums

{-
 - (drop) - Returns 2,048
 - (drop xs) - Drops the first element of xs (returns xs if not a cons)
 - (drop n xs) - Drops the first n elements of xs (n is coerced to integer)
 - {drop n m ... xs) - Drops the first n elements, then the next m, then ... in a list of lists
 - (dp) == (drop) == (cdr)
 - Example of that last one: (dp 2 3 2 '(1 2 3 4)) => '((3 4) (2 3 4) (3 4))
 -}
dropExpr :: [Expr] -> Symbols Expr Expr
dropExpr [] = pure $ Number 2048
dropExpr [x] = pure $ listCdr x
dropExpr [n, x] = pure . toExpr $ consDrop (fromExpr n) x
dropExpr xs = do
  let lst = last xs
      nums = fromExpr <$> init xs
  return . toExpr $ map (flip consDrop lst) nums

{-
 - (apply) - Returns 2,147,483,647
 - (apply f) - Applies the function to itself
 - (apply f x y ... xs) - Applies the function to the arglist
 - (ap) is an abbreviation for (apply)
 -}
applyExpr :: [Expr] -> Symbols Expr Expr
applyExpr [] = pure $ Number 2147483647
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
mapExpr [xs] = pure . expressed (reverse :: [Expr] -> [Expr]) $ xs
mapExpr (f:xss) = toExpr <$> helper (map fromExpr xss)
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
filterExpr [xs] = pure . expressed (filter fromExpr) $ xs
filterExpr (f:xs:_) = do
  let xs' = fromExpr xs
  xs'' <- filterM (\x -> fromExpr <$> functionCall f [x]) xs'
  return $ toExpr xs''

{-
 - All of the comparisons work the same.
 - (cmp x ... y) - Returns 1 if the ordering is correct, 0 otherwise
 -
 - Numeric:
 - * less-than / c,
 - * less-eq / =,
 - * greater-than / c;
 - * greater-eq / =;
 - String:
 - * str-less-than / ,c
 - * str-less-eq / ,=
 - * str-greater-than / ;c
 - * str-greater-eq / ;=
 -}

assertOrdering :: (a -> a -> Bool) -> (Expr -> a) -> [Expr] -> Bool
assertOrdering order f xs = all (uncurry order) . pairs $ map f xs

orderingOp :: (a -> a -> Bool) -> (Expr -> a) -> ([Expr] -> Symbols Expr Expr)
orderingOp order f xs  = pure . toExpr $ assertOrdering order f xs

{-
 - (range) - Returns 1337
 - (range n) - Returns from 1 to n (if n < 1, empty list)
 - (range n1 ... nm) - Returns n1..n2..n3.. ... nm
 - (rg) == (range)
 - Example: (rg 5 7 3) => '(5 6 7 6 5 4 3)
 -}
rangeExpr :: [Expr] -> Symbols Expr Expr
rangeExpr [] = pure $ Number 1337
rangeExpr [x] = pure . toExpr . map toExpr $ [1 :: Integer .. fromExpr x]
rangeExpr xs = pure . toExpr . map Number . rangeHelper $ map fromExpr xs
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
 - (cons x) = (x . x)
 - (cons x y ... z) = (x y ... . z)
 - (c) == (cons)
 -}
consExpr :: [Expr] -> Symbols Expr Expr
consExpr [] = pure $ Cons Nil Nil
consExpr [x] = pure $ Cons x x
consExpr [x, y] = pure $ Cons x y
consExpr (x:xs) = Cons x <$> consExpr xs

{-
 - (bitand) - All one-bits
 - (bitand x ...) - Bitwise and together
 - (b&) == (bitand)
 -}
andExpr :: [Expr] -> Symbols Expr Expr
andExpr = pure . Number . foldr (.&.) (complement zeroBits) . map fromExpr

{-
 - (bitor) - All zero-bits
 - (bitor x ...) - Bitwise or together
 - (b;) = (bitor)
 -}
orExpr :: [Expr] -> Symbols Expr Expr
orExpr = pure . Number . foldr (.|.) zeroBits . map fromExpr

{-
 - (bitxor) - 256
 - (bitxor x) - Complement
 - (bitxor x ...) - Bitwise xor together
 - (b%) = (bitxor)
 -}
xorExpr :: [Expr] -> Symbols Expr Expr
xorExpr [] = pure $ Number 256
xorExpr [x] = pure . Number . complement $ fromExpr x
xorExpr xs = pure . Number . foldr1 xor . map fromExpr $ xs

{-
 - (boolnorm) - 999
 - (boolnorm x) - The value x, normalized to boolean (so 0 or 1)
 - (boolnorm x ... y) - Each value normalized, put into a list
 - (bn) == (boolnorm)
 -}
boolNorm :: [Expr] -> Symbols Expr Expr
boolNorm [] = pure $ Number 999
boolNorm [x] = pure . Number $ if fromExpr x then 1 else 0
boolNorm xs = toExpr <$> mapM (boolNorm . return) xs

{-
 - (hook) - ((hook) x y z) = (list z y x)
 - (hook f) - ((hook f) x y z) = (f z y x)
 - (hook f g) - ((hook f g) x y z) = (f (g x) (g y) (g z))
 - (hook f g ... h) - ((hook f g g1 ... gn) x y ... z) = (f (g x) (g1 y) ... (gn z)) ; (Uses shorter of two lists)
 - (hk) == (hook)
 -}
hook :: [Expr] -> Symbols Expr Expr
hook [] = let t :: [Expr] -> Symbols Expr Expr
              t xs = pure . toExpr $ reverse xs
          in pure . BuiltIn $ Func t
hook [f] = let t :: [Expr] -> Symbols Expr Expr
               t xs = functionCall f $ reverse xs
           in pure . BuiltIn $ Func t
hook [f, g] = let t :: [Expr] -> Symbols Expr Expr
                  t xs = mapM (functionCall g . pure) xs >>= functionCall f
              in pure $ func t
hook (f:gs) = let t :: [Expr] -> Symbols Expr Expr
                  t xs = zipWithM (\g x -> functionCall g $ pure x) gs xs >>= functionCall f
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
 - IMPORTANT NOTE: The sorting function should not carry side effects; it will be called an
 -                 unspecified number of times.
 -}
sortExpr :: [Expr] -> Symbols Expr Expr
sortExpr [] = pure $ toExpr (Number <$> [1..10])
sortExpr [xs] = pure . (expressed $ sortBy (compare `on` (fromExpr :: Expr -> Integer))) $ xs
sortExpr [f, xs] = toExpr <$> sortByM order (fromExpr xs)
    where order x y = do
            le <- fromExpr <$> functionCall f [x, y]
            ge <- fromExpr <$> functionCall f [y, x]
            pure $ case (le, ge) of
                     (True, True) -> EQ
                     (True, False) -> LT
                     (False, True) -> GT
                     (False, False) -> EQ -- Chosen arbitrarily
sortExpr (f:xs) = sortExpr [f, toExpr xs]

{-
 - (divides) - Returns -100
 - (divides x) - Returns whether x is a power of 10
 - (divides x y z ... t) - Returns whether x | y and y | z and z | ... | t
 - (//) == (divides)
 - Division by zero always results in a false expression
 -}
divides :: [Expr] -> Symbols Expr Expr
divides [] = pure $ Number (-100)
divides [x] = let check :: Integer -> Bool
                  check 0 = False
                  check 1 = True
                  check n = check $ n `div` 10
              in pure . expressed check $ x
divides [x, y] = let x' = fromExpr x :: Integer
                     y' = fromExpr y :: Integer
                 in case () of
                      _ | x' == 0 -> pure false
                        | (y' `div` x') * x' == y' -> pure true
                        | otherwise -> pure false
divides (x:y:xs) = do
  a1 <- divides [x, y]
  a2 <- divides (y:xs)
  return $ toExpr (fromExpr a1 && fromExpr a2)

{-
 - (foldl) - Returns 1,000,000
 - (foldl xs) - Sums the list
 - (foldl f x ... y xs) - Perform binary operation f on the list (x ... y . xs)
 - (fl) == (foldl)
 - In the third case, if the list is empty and no extra arguments are supplied, returns nil
 -}
foldlExpr :: [Expr] -> Symbols Expr Expr
foldlExpr [] = pure $ Number 1000000
foldlExpr [xs] = pure . Number . sum . map fromExpr $ fromExpr xs
foldlExpr (f:ys) = case init ys ++ fromExpr (last ys) of
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
foldrExpr [xs] = pure . Number . product . map fromExpr $ fromExpr xs
foldrExpr (f:ys) = case init ys ++ fromExpr (last ys) of
                     [] -> return Nil
                     xs -> foldrM (\x y -> functionCall f [x, y]) (last xs) (init xs)

{-
 - (join) - ((join) x y z) = (list x x y y z z)
 - (join f) - ((join f) x y z) = (f x x y y z z)
 - (join f x ... y) - ((join f) x ... y)
 - (jn) == (join)
 -}
joinExpr :: [Expr] -> Symbols Expr Expr
joinExpr [] = let t xs = pure . (toExpr :: [Expr] -> Expr) . concatMap (\x -> [x, x]) $ xs
              in pure . BuiltIn $ Func t
joinExpr [f] = let t xs = functionCall f $ concatMap (\x -> [x, x]) xs
               in pure . BuiltIn $ Func t
joinExpr (f:xs) = joinExpr [f] >>= \f' -> functionCall f' xs

{-
 - (mod) - 500
 - (mod x) - Return x `mod` 10
 - (mod x y . xs) - Return x `mod` y
 - (md) == (mod)
 - Modulo by zero yields the dividend
 - Note that the sign of the modulo always matches the sign of the divisor
 -}
modExpr :: [Expr] -> Symbols Expr Expr
modExpr [] = pure $ Number 500
modExpr [x] = pure . Number . (`mod` 10) . fromExpr $ x
modExpr (x:y:_) = pure . Number $ (mod' `on` fromExpr) x y
    where mod' a 0 = a
          mod' a b = mod a b

{-
 - (even) - Returns 64
 - (even x) - Returns true if even
 - (even . xs) - Returns a list of booleans
 - (ev) == (even)
 -}
evenExpr :: [Expr] -> Symbols Expr Expr
evenExpr [] = pure $ Number 64
evenExpr [x] = pure . expressed (even :: Integer -> Bool) $ x
evenExpr xs = pure . toExpr . map (expressed (even :: Integer -> Bool)) $ xs

{-
 - (odd) - Returns 32
 - (odd x) - Returns true if odd
 - (odd . xs) - Returns a list of booleans
 - (od) == (odd)
 -}
oddExpr :: [Expr] -> Symbols Expr Expr
oddExpr [] = pure $ Number 32
oddExpr [x] = pure . expressed (odd :: Integer -> Bool) $ x
oddExpr xs = pure . toExpr . map (expressed $ (odd :: Integer -> Bool)) $ xs

{-
 - (strings) - Returns the empty string
 - (strings . xs) - Concatenate strings
 - (s) == (strings)
 -}
stringConcat :: [Expr] -> Symbols Expr Expr
stringConcat = pure . String . fold . map fromExpr

{-
 - (prime) - Returns 128
 - (prime x) - Returns true if abs(x) is prime
 - (prime . xs) - Returns a list of booleans
 - (pm) == (prime)
 -}
primeExpr :: [Expr] -> Symbols Expr Expr
primeExpr [] = pure $ Number 128
primeExpr [x] = pure . expressed (isPrime :: Integer -> Bool) $ x
primeExpr xs = pure . toExpr . map (expressed (isPrime :: Integer -> Bool)) $ xs

{-
 - (up) - Returns 9,999
 - (up x) - Returns x^2
 - (up x y) - Returns x^|y| (TODO Support negative exponents)
 - (up x ... y) - Uses right-tower
 - For these purposes, 0^0 = 1
 -}
powerExpr :: [Expr] -> Symbols Expr Expr
powerExpr [] = pure $ Number 9999
powerExpr [x] = pure . Number . join (*) . fromExpr $ x
powerExpr xs = let xs' = map fromExpr xs
                   h x y = x ^ abs y
               in pure . Number $ foldr1 h xs'

{-
 - (mo) - Returns % minus 1
 - (mo . xs) - Equivalent to (m ,@xs 1)
 -}
minusOne :: [Expr] -> Symbols Expr Expr
minusOne [] = implicitValue >>= \x -> minusOne [x]
minusOne xs = minus $ xs ++ [Number 1]

{-
 - (po) - Returns % plus 1
 - (po . xs) - Equivalent to (p ,@xs 1)
 -}
plusOne :: [Expr] -> Symbols Expr Expr
plusOne [] = implicitValue >>= \x -> plusOne [x]
plusOne xs = plus $ xs ++ [Number 1]

{-
 - (mt) - Returns % minus 2
 - (mt . xs) - Equivalent to (m ,@xs 2)
 -}
minusTwo :: [Expr] -> Symbols Expr Expr
minusTwo [] = implicitValue >>= \x -> minusTwo [x]
minusTwo xs = minus $ xs ++ [Number 2]

{-
 - (pt) - Returns % plus 2
 - (pt . xs) - Equivalent to (p ,@xs 2)
 -}
plusTwo :: [Expr] -> Symbols Expr Expr
plusTwo [] = implicitValue >>= \x -> plusTwo [x]
plusTwo xs = plus $ xs ++ [Number 2]

{-
 - (split) - Returns the digits 0-9 in a string
 - (split x) - Splits the string x into a list, delimited at spaces
 - (split x "") - Splits the string into individual characters
 - (split x d) - Splits the string x into a list, delimited by d
 - (split x y ... z d) - Splits each string into lists, flattening
 - (sp) == (split)
 -}
splitExpr :: [Expr] -> Symbols Expr Expr
splitExpr [] = pure $ String "0123456789"
splitExpr [x] = splitExpr [x, String " "]
splitExpr [x, d] | null (fromExpr d :: String) = let f :: String -> [Expr]
                                                     f = map (\y -> toExpr [y])
                                                 in pure $ expressed f x
splitExpr [x, d] = pure . toExpr . map String $ unintercalate (fromExpr d) (fromExpr x)
splitExpr xs = let xs' = init xs
                   d = last xs
               in toExpr <$> mapM (\x -> splitExpr [x, d]) xs'

{-
 - (inter) - Returns ")!@#$%^&*(" in a string
 - (inter xs) - Joins the list xs into a single string, delimited by spaces
 - (inter xs d) - Joins the list, using the given delimiter
 - (inter xs ys ... zs d) - Joins all of the lists, collectively
 - (ps) == (inter)
 -}
interExpr :: [Expr] -> Symbols Expr Expr
interExpr [] = pure $ String ")!@#$%^&*("
interExpr [x] = interExpr [x, String " "]
interExpr [x, d] = pure . String $ intercalate (fromExpr d) (map fromExpr $ fromExpr x)
interExpr xs = let xs' = init xs
                   d = fromExpr $ last xs
               in pure . String . intercalate d . map fromExpr $ concatMap fromExpr xs'

{-
 - (insert) - Returns 0 (TODO Change this)
 - (insert xs) - Returns (,xs ... ())
 - (insert x xs) - Returns (,xs ... x)
 - (insert n x xs) - Returns xs with x inserted at the nth position)
 - (insert n x ... z xs) - Returns (insert n x (insert ... z xs))
 - (it) == (insert)
 - The argument xs is always coerced to a list
 -}
insertExpr :: [Expr] -> Symbols Expr Expr
insertExpr [] = pure $ Number 0
insertExpr [xs] = insertExpr [Nil, xs]
insertExpr [x, xs] = pure . expressed (++ [x]) $ xs
insertExpr [n, x, xs] = pure . expressed (insertAt (fromExpr n :: Integer) x) $ xs
insertExpr (x:y:xs) = do
  inner <- insertExpr xs
  insertExpr [x, y, inner]

{-
 - (remove) - Returns 0 (TODO Change this)
 - (remove xs) - Removes the last element of xs
 - (remove n ... m xs) - Removes mth, then ..., then nth of xs
 - (rm) == (remove)
 - The argument xs is always coerced to a list
 -}
removeExpr :: [Expr] -> Symbols Expr Expr
removeExpr [] = pure $ Number 0
removeExpr [xs] = let init' :: [Expr] -> [Expr]
                      init' [] = []
                      init' as = init as
                  in pure . expressed init' $ xs
removeExpr [n, xs] = pure . expressed ((removeAt :: Integer -> [Expr] -> [Expr]) $ fromExpr n) $ xs
removeExpr (x:xs) = do
  inner <- removeExpr xs
  removeExpr [x, inner]

{-
 - (nth) - Returns 0 (TODO Change this)
 - (nth xs) - Gets the last element of xs
 - (nth n xs) - Returns the nth element
 - (nth n ... m xs) - Returns a list containing selected elements
 - (nh) == (nth)
 - Index is always done (mod lenxs), if list is empty, nil is returned
 -}
nthExpr :: [Expr] -> Symbols Expr Expr
nthExpr [] = pure $ Number 0
nthExpr [xs] = let last' [] = Nil
                   last' as = last as
               in pure . expressed last' $ xs
nthExpr [n, xs] = let n' = fromExpr n :: Integer
                      xs' = fromExpr xs :: [Expr]
                  in pure . maybe Nil id $ xs' `wrappedNth` n'
nthExpr ms = let ns = init ms
                 xs = last ms
             in toExpr <$> mapM (\n -> nthExpr [n, xs]) ns

{-
 - (read) - Nil
 - (read x) - Reads the string as an S-expression
 - (read x ... y) - Reads each expression; returns a list
 - (rd) == (read)
 -}
readStmt :: [Expr] -> Symbols Expr Expr
readStmt [] = pure Nil
readStmt [x] = pure $ case readSingleExpr $ fromExpr x of
                        Left s -> String s -- TODO Something error-related here
                        Right y -> y
readStmt xs = toExpr <$> mapM (\x -> readStmt [x]) xs

{-
 - (eval) - Nil
 - (eval x ... y) - Evaluates the expressions in sequence
 -}
evalStmt :: [Expr] -> Symbols Expr Expr
evalStmt = evalSeq

{-
 - (two-curry) - Returns 1,000
 - (two-curry f) - (((two-curry f) x y ... z) a b ... c) => (f x y ... z a b ... c)
 - (two-curry f x y .... z) - ((two-curry f x y .... z) a b ... c) => (f x y ... z a b ... c)
 - (tc) == (two-curry)
 -}
twoCurry :: [Expr] -> Symbols Expr Expr
twoCurry [] = pure $ Number 1000
twoCurry [f] = let t = pure . BuiltIn . Func . s
                   s xs ys = functionCall f (xs ++ ys)
               in pure . BuiltIn . Func $ t
twoCurry (f:xs) = twoCurry [f] >>= \f' -> functionCall f' xs

{-
 - (seq-while) - Returns 0 (TODO Change this)
 - (seq-while seq) - Given a sequence from (sequence), return the first hundred elements in a list
 - (seq-while seq f) - Given a sequence, return the longest prefix whose elements satisfy f
 - (seq-while seq f ... g) - Return the longest prefix satisfying f ... g
 - (sw) == (seq-while)
 -}
seqWhile :: [Expr] -> Symbols Expr Expr
seqWhile [] = pure $ Number 0
seqWhile [xs] = fmap toExpr $ sequence [functionCall xs [Number n] | n <- [0..99]]
seqWhile (xs : fs) = let cond x = fmap and $ mapM (\f -> fromExpr <$> functionCall f [x]) fs
                         stream = [functionCall xs [Number n] | n <- [0..]]
                     in toExpr <$> (join . fmap sequence $ takeWhileM (>>= cond) stream)

{-
 - (print-greek) - Returns 0 (TODO Change this)
 - (print-greek n) - Prints the nth letter of the Greek alphabet
 - (print-greek n ... m) - Does (print-greek i) for each n ... m
 - (pgk) == (print-greek)
 -}
printGreek :: [Expr] -> Symbols Expr Expr
printGreek [] = pure $ Number 0
printGreek [n] = let i = fromInteger (fromExpr n) `mod` length greek
                 in Nil <$ (liftIO . putStrLn $ greek !! i)
printGreek ns = Nil <$ mapM (printGreek . return) ns

{-
 - (each-char) - Returns 0 (TODO Change this)
 - (each-char s) - Perform ROT-13 on s
 - (each-char s f) - Execute f on each character's ASCII value to get a new ASCII value
 - (each-char s n) - Perform ROT-n on s
 - (each-char s f ... g) - Cycle through f ... g for the characters
 - (ec) == (each-char)
 -}
eachChar :: [Expr] -> Symbols Expr Expr
eachChar [] = pure $ Number 0
eachChar [s] = pure $ expressed (map $ rotateChar 13) s
eachChar [s, Number n] = pure $ expressed (map . rotateChar $ fromInteger n) s
eachChar (s : fs) = let operation f x = (chr' . fromExpr) <$> functionCall f [toExpr $ ord x]
                    in expressedM (sequence . zipWith operation (cycle fs)) s

{-
 - (string-replace) - Returns 0 (TODO Change this)
 - (string-replace x) - Removes all characters which are not alphanumeric or underscore
 - (string-replace x y) - Removes any instance of the string y in x
 - (string-replace x y z) - Replaces any instance of y in x with the result of 0-ary function z
 - (string-replace x y z . t) - Ignores the remaining arguments (TODO Change this)
 - (sr) == (string-replace)
 -}
stringRepl :: [Expr] -> Symbols Expr Expr
stringRepl [] = pure $ Number 0
stringRepl [x] = pure $ expressed (filter (liftM2 (||) isAlphaNum (== '_'))) x
stringRepl [x, y] = pure . toExpr $ replaceString (fromExpr x) (fromExpr y) ""
stringRepl [x, y, z] = let oper :: Symbols Expr String
                           oper = fromExpr <$> functionCall z []
                       in toExpr <$> replaceStringM (fromExpr x) (fromExpr y) oper
stringRepl xs = stringRepl $ take 3 xs

{-
 - All of the casing operators work the same.
 - (casing) - Converts % to the appropriate case
 - (casing x) - Converts the argument to the specified case, using standard casing rules
 - (casing x y . xs) - Converts each argument to the specified case, returning a list
 -
 - Cases:
 - * uc  - Standard uppercase
 - * lc  - Standard lowercase
 - * ucx - Uppercase using ShinyLisp rules
 - * lcx - Lowercase using ShinyLisp rules
 -}
caseOp :: (Char -> Char) -> [Expr] -> Symbols Expr Expr
caseOp op [] = expressed (map op) <$> implicitValue
caseOp op [x] = pure $ expressed (map op) x
caseOp op xs = pure . toExpr $ map (expressed $ map op) xs

{-
 - (sum-digits) - Returns 0 (TODO Change this)
 - (sum-digits x) - Sums the digits of x
 - (sum-digits x . y) - Sums the digits of each value and sums the values
 - (sd) == (sum-digits)
 - Note: Negative numbers are always absolute-valued before applying this function
 -}
sumDigits :: [Expr] -> Symbols Expr Expr
sumDigits [] = pure $ Number 0
sumDigits xs = let digits :: Integer -> [Integer]
                   digits = map (toInteger . subtract (ord '0') . ord) . show . abs
               in pure . toExpr . sum . map (sum . digits . fromExpr) $ xs

{-
 - (gets) - Reads a single line of input from the user, returning a string
 - (gets n) - Reads n lines of input from the user, returning a list of strings (even if n = 1)
 - (gets n . m) - Reads n lines of input
 - (ge) == (gets)
 -}
gets :: [Expr] -> Symbols Expr Expr
gets [] = toExpr <$> liftIO getLine
gets [n] = liftIO . fmap (toExpr . map toExpr) $ replicateM (fromExpr n) getLine
gets (n:_) = gets [n]

{-
 - (- . forms) - Evaluates the forms for each line of input provided, returning the final value
 -   This is a general looping construct designed for convenient IO programs. For each line of input,
 -   the variable % is bound to the line of input, then the forms are executed, then % is printed to
 -   the screen. At the end of the final iteration, if % was bound by the loop, then % is unbound to
 -   prevent a redundant print.
 -}
interaction :: [Expr] -> Symbols Expr Expr
interaction xs = do
  bound <- hasSymbol implicitName
  result <- loop
  unless bound $ undefSymbol implicitName
  return $ case result of
             Nothing -> Nil
             Just z -> z
    where loop = do
            done <- liftIO isEOF
            if done then
                return Nothing
            else do
              y <- liftIO getLine
              setOrDefSymbol implicitName $ toExpr y
              val <- evalSeq xs
              percent <- getSymbolMaybe implicitName
              case percent of
                Nothing -> pure ()
                Just x -> userPrint x >>= liftIO . putStrLn
              done1 <- liftIO isEOF
              if done1 then
                  return $ Just val
              else
                  loop

{-
 - (print) - Prints %
 - (print . any) - Prints each element (evaluated), on its own line; returns Nil
 - (print) == (pn)
 - Note that (print) prints a representation-friendly form while (puts) prints a user-friendly form
 -}
putsPrint :: [Expr] -> Symbols Expr Expr
putsPrint [] = do
  value <- implicitValue
  userPrint value >>= liftIO . putStrLn
  return Nil
putsPrint xs = do
  forM_ xs $ \x -> liftIO . putStrLn $ printable x
  return Nil

{-
 - (define) - Binds the variable % in the local scope
 - (define x) - Binds the variable x (not evaluated) in the local scope, returning its value
 - (define x ... y) - Binds each variable locally, returning the value of the last one
 - (define) == (,-)
 -}
defineVar :: [Expr] -> Symbols Expr Expr
defineVar [] = defineVar [Atom "%"]
defineVar xs = do
  result <- forM (toVars xs) $ \x -> do
                 prev <- getSymbolOrDefault x Nil
                 defSymbol x prev
                 return prev
  -- We know that xs is nonempty since the [] case is handled above, so result is nonempty
  return $ last result

{-
 - (undefine) - Unbinds the tightest binding of %
 - (undefine x ... y) - Unbinds each variable (not evaluated), skipping any which do not exist
 - (undefine) == (-,)
 -}
undefineVar :: [Expr] -> Symbols Expr Expr
undefineVar [] = undefineVar [Atom "%"]
undefineVar xs = do
  forM_ (toVars xs) $ \x -> do
                   exists <- hasSymbol x
                   when exists $ undefSymbol x
  return Nil

{-
 - (argv) - Returns the command line arguments, as a list
 - (argv n) - Returns the nth command line argument (wrapping if necessary)
 - (argv n ... m) - Returns a list containing each argument
 - (argv) == (av)
 -}
getArgv :: [Expr] -> Symbols Expr Expr
getArgv [] = toExpr . map toExpr <$> liftIO getArgs
getArgv ns = do
  args <- liftIO getArgs
  let ns' = map fromExpr ns :: [Integer]
      result = map (wrappedNth args) ns'
      result' = map (maybe Nil toExpr) result
  return $ case result' of
             [x] -> x
             xs  -> toExpr xs
