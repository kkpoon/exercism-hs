module SpaceAge (Planet(..), ageOn) where

data Planet
    = Earth
    | Mercury
    | Venus
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune

orbitalPeriod :: Planet -> Float
orbitalPeriod planet =
    case planet of
        Earth -> 1.0
        Mercury -> 0.2408467
        Venus -> 0.61519726
        Mars -> 1.8808158
        Jupiter -> 11.862615
        Saturn -> 29.447498
        Uranus -> 84.016846
        Neptune -> 164.79132

earthSecondToYear :: Float -> Float
earthSecondToYear seconds = seconds / 31557600.0

ageOn :: Planet -> Float -> Float
ageOn planet seconds = (earthSecondToYear seconds) / (orbitalPeriod planet)

