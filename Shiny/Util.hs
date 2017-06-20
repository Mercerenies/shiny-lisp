
module Shiny.Util(sortByM, isPrime, unintercalate, insertAt, removeAt, wrappedNth, takeWhileM,
                  rotateChar, chr', replaceString, replaceStringM) where

import Data.Char
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
isPrime p | p < 0 = isPrime $ abs p
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

rotateChar :: Int -> Char -> Char
rotateChar 0 ch = ch
rotateChar n ch | isAsciiUpper ch = chr $ (ord ch - ord 'A' + n) `mod` 26 + ord 'A'
rotateChar n ch | isAsciiLower ch = chr $ (ord ch - ord 'a' + n) `mod` 26 + ord 'a'
rotateChar _ ch = ch

chr' :: Int -> Char
chr' n | n >= ord minBound && n <= ord maxBound = chr n
       | otherwise = chr 0

replaceString :: Eq a => [a] -> [a] -> [a] -> [a]
replaceString [] _  _  = []
replaceString as bs cs
    | take (length bs) as == bs = cs ++ replaceString (drop (length bs) as) bs cs
    | otherwise = head as : replaceString (tail as) bs cs

replaceStringM :: (Applicative m, Eq a) => [a] -> [a] -> m [a] -> m [a]
replaceStringM [] _  _   = pure []
replaceStringM as bs mcs
    | take (length bs) as == bs = (++) <$> mcs <*> replaceStringM (drop (length bs) as) bs mcs
    | otherwise = (head as :) <$> replaceStringM (tail as) bs mcs
