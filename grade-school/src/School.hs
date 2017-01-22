module School (School, add, empty, grade, sorted) where

import           Data.List (sort)
import           Data.Map  (Map, (!))
import qualified Data.Map  as Map

data School = School (Map Int [String])

add :: Int -> String -> School -> School
add gradeNum student (School school) =
    School $ Map.insert gradeNum newStudents school
        where
            existingStudents = grade gradeNum $ School school
            newStudents = sort $ student:existingStudents


empty :: School
empty = School Map.empty

grade :: Int -> School -> [String]
grade gradeNum (School school) =
    case Map.lookup gradeNum school of
        Just students -> students
        Nothing       -> []

sorted :: School -> [(Int, [String])]
sorted (School school) = Map.toAscList school
