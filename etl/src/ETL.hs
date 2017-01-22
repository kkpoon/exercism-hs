module ETL (transform) where

import           Data.Char (toLower)
import           Data.Map  (Map, fromList, toList)

transform :: Map a String -> Map Char a
transform =
    fromList . concatMap toNewFormat . toList
        where
            toNewFormat (x, y) = zip (map toLower y) (replicate (length y) x)
