module Bob (responseFor) where

import Data.Char
import qualified Data.Text as T

strip :: String -> String
strip =
    T.unpack . T.strip . T.pack

isSayingNothing :: String -> Bool
isSayingNothing input =
    strip input == ""

isYell :: String -> Bool
isYell input =
    input == (map toUpper input) && or (map isAlpha input)
    
isAsking :: String -> Bool
isAsking input =
    last (strip input) == '?'
    
responseFor :: String -> String
responseFor input
    | isSayingNothing input = "Fine. Be that way!"
    | isYell input = "Whoa, chill out!"
    | isAsking input = "Sure."
    | otherwise = "Whatever."
