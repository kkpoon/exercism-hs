module Phone (areaCode, number, prettyPrint) where

import           Data.Char (isDigit)
import           Data.List (length, splitAt)

areaCode :: String -> Maybe String
areaCode num =
    case number num of
        Just n ->
            Just (take 3 n)
        _ ->
            Nothing

number :: String -> Maybe String
number num =
    case (len, n) of
        (10, _) ->
            Just n
        (11, ('1': xs)) ->
            Just xs
        _ ->
            Nothing
    where
        n = filter isDigit num
        len = length n

prettyPrint :: String -> Maybe String
prettyPrint num =
    case number num of
        Just n ->
            let
                three = splitAt 3
                sp = three n
                code = fst sp
                (p1, p2) = three (snd sp)
            in
                Just ("(" ++ (code) ++ ") " ++ p1 ++ "-" ++ p2)

        Nothing ->
            Nothing
