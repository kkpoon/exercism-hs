module Scrabble (scoreLetter, scoreWord) where

import           Data.Char

scoreLetter :: Char -> Int
scoreLetter input
    | elem c "AEIOULNRST" = 1
    | elem c "DG" = 2
    | elem c "BCMP" = 3
    | elem c "FHVWY" = 4
    | elem c "K" = 5
    | elem c "JX" = 8
    | elem c "QZ" = 10
    | otherwise = 0
    where c = toUpper input

scoreWord :: String -> Int
scoreWord =
    sum . map scoreLetter
