
module Shiny.Util(sortByM, isPrime, unintercalate, insertAt, removeAt, wrappedNth, takeWhileM) where

import Data.List
import Control.Monad

sortByM :: Monad m => (a -> a -> m Ordering) -> [a] -> m [a]
sortByM _ [] = pure []
sortByM cmp (x:xs) = do
  less <- filterM (\y -> (/= GT) <$> y `cmp` x ) xs
  greater <- filterM (\y -> (== GT) <$> y `cmp` x) xs
  less' <- sortByM cmp less
  greater' <- sortByM cmp greater
  return $ less' ++ [x] ++ greater'

isPrime :: Integral a => a -> Bool
isPrime p | p < 2 = False
isPrime p = all (\x -> p `mod` x /= 0) [2..p-1]

unintercalate :: Eq a => [a] -> [a] -> [[a]]
unintercalate d xs = reverse $ helper xs [] []
    where len = length d
          helper [] z zs = reverse z : zs
          helper ys z zs
              | take len ys == d = helper (drop len ys) [] (reverse z : zs)
              | otherwise = helper (tail ys) (head ys : z) zs

insertAt :: Integral i => i -> a -> [a] -> [a]
insertAt n x xs = let (ls, rs) = genericSplitAt n xs in ls ++ [x] ++ rs

removeAt :: Integral i => i -> [a] -> [a]
removeAt n xs | n < 0 = xs
removeAt n xs = case genericSplitAt n xs of
                  (ls, _:rs) -> ls ++ rs
                  (ls, rs) -> ls ++ rs

wrappedNth :: Integral i => [a] -> i -> Maybe a
wrappedNth [] _ = Nothing
wrappedNth xs n = Just $ xs `genericIndex` (n `mod` genericLength xs)

takeWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
takeWhileM _ [] = pure []
takeWhileM p (x:xs) = do
  px <- p x
  if px then
      (x :) <$> takeWhileM p xs
  else
      pure []
