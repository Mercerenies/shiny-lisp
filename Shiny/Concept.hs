
module Shiny.Concept(Sequence(), smap, smap', seqFromList, seqTake, seqTakeWhile,
                     PSequence(), psmap, psmap', pseqFromList) where

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

-- /////
