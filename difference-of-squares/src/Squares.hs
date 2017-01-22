module Squares (difference, squareOfSums, sumOfSquares) where

difference :: Integral a => a -> a
difference num = (squareOfSums num) - (sumOfSquares num)

squareOfSums :: Integral a => a -> a
squareOfSums num = (sum [1..num]) ^ 2

sumOfSquares :: Integral a => a -> a
sumOfSquares num = sum $ map (\n -> n * n) [1..num]
