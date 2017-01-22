module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard predicate a = [x | x <- a, not $ predicate x]

keep :: (a -> Bool) -> [a] -> [a]
keep predicate a = [x | x <- a, predicate x]
