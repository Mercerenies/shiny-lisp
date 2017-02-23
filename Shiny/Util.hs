
module Shiny.Util(sortByM, isPrime, unintercalate) where

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
