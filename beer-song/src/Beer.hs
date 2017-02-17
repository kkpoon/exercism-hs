module Beer (song) where

import           Text.Printf

songLine :: Int -> String
songLine num
    | num > 2 =
        (printf "%d bottles of beer on the wall, %d bottles of beer.\n\
        \Take one down and pass it around, %d bottles of beer on the wall.\n\
        \\n" num num (num - 1)) ++ (songLine (num - 1))
    | num == 2 =
        "2 bottles of beer on the wall, 2 bottles of beer.\n\
        \Take one down and pass it around, 1 bottle of beer on the wall.\n\
        \\n" ++ (songLine (num - 1))
    | num == 1 =
        "1 bottle of beer on the wall, 1 bottle of beer.\n\
        \Take it down and pass it around, no more bottles of beer on the wall.\n\
        \\n" ++ (songLine (num - 1))
    | otherwise =
        "No more bottles of beer on the wall, no more bottles of beer.\n\
        \Go to the store and buy some more, 99 bottles of beer on the wall.\n"

song :: String
song =
    songLine 99

