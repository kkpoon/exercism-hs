module DNA (toRNA) where

toRNA :: String -> Maybe String
toRNA input =
    if length input == (length . filter badFilter) input then
        Just (map dnaToRNA input)
    else
        Nothing
    where
        badFilter x =
            case x of
                'G' -> True
                'C' -> True
                'T' -> True
                'A' -> True
                _ -> False
        dnaToRNA x =
            case x of
                'G' -> 'C'
                'C' -> 'G'
                'T' -> 'A'
                'A' -> 'U'
                _ -> 'X'
