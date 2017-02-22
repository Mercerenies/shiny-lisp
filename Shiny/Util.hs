
module Shiny.Util(sortByM) where

import Control.Monad

sortByM :: Monad m => (a -> a -> m Ordering) -> [a] -> m [a]
sortByM _ [] = pure []
sortByM cmp (x:xs) = do
  less <- filterM (\y -> (/= GT) <$> y `cmp` x ) xs
  greater <- filterM (\y -> (== GT) <$> y `cmp` x) xs
  less' <- sortByM cmp less
  greater' <- sortByM cmp greater
  return $ less' ++ [x] ++ greater'
