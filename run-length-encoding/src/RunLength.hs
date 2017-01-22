module RunLength (decode, encode) where

import           Data.Char
import           Data.List

decoder :: String -> String -> String
decoder [] _ = []
decoder [x] "" = [x]
decoder [x] num = replicate (read num) x
decoder (x:xs) num
    | isDigit x = decoder xs (num ++ [x])
    | num == "" = x : decoder xs ""
    | otherwise = replicate (read num) x ++ (decoder xs "")

decode :: String -> String
decode input = decoder input ""

encode :: String -> String
encode = concatMap mapper . group
    where
        printer n s = if n == 1 then [head s] else (show n) ++ [head s]
        mapper grp = printer (length grp) grp

