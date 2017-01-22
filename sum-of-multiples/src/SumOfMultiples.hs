module SumOfMultiples (sumOfMultiples) where

import Data.List (unfoldr, nub)

sumOfMultiples :: [Int] -> Int -> Int
sumOfMultiples factors limit = sum $ nub $ concatMap multiples factors
    where
        multiples factor =
            unfoldr
                (\f -> if f >= limit then Nothing else Just (f, f + factor))
                factor
