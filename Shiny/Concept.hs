
module Shiny.Concept(Sequence(), smap, smap', seqFromList, seqTake, seqTakeWhile,
                     PSequence(), psmap, psmap', pseqFromList,
                     Grid, gridmap, gridmap', gridDims, functionGrid, constGrid,
                     gridGet, gridPut) where

import Shiny.Structure
import Shiny.Symbol
import Shiny.Util
import Data.List(genericIndex, genericTake)
import Control.Monad

-- The Sequence Concept --

-- (seq) ==> (seq 0)
-- (seq n) ==> The nth element of the sequence

newtype Sequence = Sequence Function

instance FromExpr Sequence where
    fromExpr = Sequence . fromExpr

instance ToExpr Sequence where
    toExpr (Sequence x) = toExpr x

smap :: (FromExpr a, ToExpr b) => (a -> b) -> Sequence -> Sequence
smap f = smap' (expressed f)

smap' :: (Expr -> Expr) -> Sequence -> Sequence
smap' f (Sequence g) = Sequence $ \x -> f <$> g x

seqFromList :: [Expr] -> Sequence
seqFromList xs = Sequence f
    where protect n = case (fromExpr n :: Integer) of
                        n' | n' < 0    -> 0
                           | otherwise -> n'
          f []    = f [Number 0]
          f (n:_) = pure $ xs `genericIndex` protect n

seqTake :: Integer -> Sequence -> Symbols Expr [Expr]
seqTake n (Sequence f) = sequence [f [Number i] | i <- [0..n-1]]

seqTakeWhile :: (Expr -> Symbols Expr Bool) -> Sequence -> Symbols Expr [Expr]
seqTakeWhile p (Sequence f) = let stream = [f [Number n] | n <- [0..]]
                              in join . fmap sequence . takeWhileM (>>= p) $ stream

-- The Prefix Sequence Concept --

-- (pseq) ==> (pseq 1)
-- (pseq n) ==> The first n elements of the sequence, as a list

newtype PSequence = PSequence Function

instance FromExpr PSequence where
    fromExpr = PSequence . fromExpr

instance ToExpr PSequence where
    toExpr (PSequence x) = toExpr x

psmap :: (FromExpr a, ToExpr b) => (a -> b) -> PSequence -> PSequence
psmap f = psmap' (expressed f)

psmap' :: (Expr -> Expr) -> PSequence -> PSequence
psmap' f (PSequence g) = PSequence $ \x -> expressed (map f) <$> g x

pseqFromList :: [Expr] -> PSequence
pseqFromList xs = PSequence f
    where protect n = case (fromExpr n :: Integer) of
                        n' | n' < 0    -> 0
                           | otherwise -> n'
          f []    = f [Number 1]
          f (n:_) = pure . toExpr $ genericTake (protect n) xs

-- The Grid Concept --

-- (grd) ==> (width height)
-- (grd nm) ==> The (nth, mth) element

-- NOTE: Grids are always indexed using 0-based row-major indices
-- NOTE: For out-of-bounds indices, () should be returned

newtype Grid = Grid Function

instance FromExpr Grid where
    fromExpr = Grid . fromExpr

instance ToExpr Grid where
    toExpr (Grid x) = toExpr x

gridmap :: (FromExpr a, ToExpr b) => (a -> b) -> Grid -> Grid
gridmap f = gridmap' (expressed f)

gridmap' :: (Expr -> Expr) -> Grid -> Grid
gridmap' f (Grid g) = Grid $ \x -> f <$> g x

gridDims :: Grid -> Symbols Expr (Int, Int)
gridDims (Grid g) = helper . fromExpr <$> g []
    where helper [w, h] = (fromExpr w, fromExpr h)
          helper _ = (0, 0)

functionGrid :: (Int, Int) -> ((Int, Int) -> Expr) -> Grid
functionGrid (h, w) f = Grid $ return . g
    where g [] = toExpr [toExpr h, toExpr w]
          g (r:c:_) = f (fromExpr r, fromExpr c)
          g _ = Nil

constGrid :: (Int, Int) -> Expr -> Grid
constGrid (h, w) expr = functionGrid (h, w) g
    where g (r, c)
              | r < 0 || r >= h = Nil
              | c < 0 || c >= w = Nil
              | otherwise       = expr

gridGet :: Grid -> (Int, Int) -> Symbols Expr Expr
gridGet (Grid g) (n, m) = g [toExpr list]
    where list = [toExpr n, toExpr m]

gridPut :: (Int, Int) -> Expr -> Grid -> Grid
gridPut (n, m) expr (Grid g) = Grid g'
    where g' (r:c:_)
              | fromExpr r == n && fromExpr c == m = pure expr
          g' xs = g xs
