module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance [] [] = Just 0
distance [x] [y]
    | x == y = Just 0
    | x /= y = Just 1
distance (x:xs) (y:ys)
    | length xs /= length ys = Nothing
    | x == y = distance xs ys
    | x /= y =
        case distance xs ys of
            Just d  -> Just (1 + d)
            Nothing -> Nothing
