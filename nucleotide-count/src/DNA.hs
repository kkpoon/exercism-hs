module DNA (count, nucleotideCounts) where

import           Data.Map (Map, fromList)

count :: Char -> String -> Either String Int
count _ [] = Right 0
count n (d:na) =
    if (elem n nucleotides) && (elem d nucleotides) then
        case count n na of
            Right cnt -> Right (matchCnt + cnt)
            Left err  -> Left err
    else
        Left "bad nucleotides"
    where
        matchCnt = if n == d then 1 else 0
        nucleotides = ['A', 'C', 'G', 'T']

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts dna =
    case (a,c,g,t) of
        (Right aa, Right cc, Right gg, Right tt) ->
            Right (fromList [('A', aa), ('C', cc), ('G', gg), ('T', tt)])
        _ ->
            Left "Invalid DNA"
    where
        a = count 'A' dna
        c = count 'C' dna
        g = count 'G' dna
        t = count 'T' dna
