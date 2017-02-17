module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: Float -> Float -> Float -> TriangleType
triangleType a b c
    | a == 0 || b == 0 || c == 0 = Illegal
    | a + b < c || a + c < b || b + c < a = Illegal
    | a == b && b == c = Equilateral
    | a == b || b == c || a == c = Isosceles
    | otherwise = Scalene
