module Raindrops (convert) where

import           Data.List (intercalate)

convert :: Int -> String
convert n =
    if drops == "" then
        show n
    else
        drops
    where
        drops = intercalate "" [sound x | x <- [1..n], mod n x == 0]
        sound n =
            case n of
                3 -> "Pling"
                5 -> "Plang"
                7 -> "Plong"
                _ -> ""
