module Accumulate (accumulate) where

accumulate :: (a -> b) -> [a] -> [b]
accumulate fn input = [fn x | x <- input]
