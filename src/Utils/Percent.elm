module Utils.Percent exposing (..)

import Quantity exposing (Quantity(..))


type Percentile
    = Percentile


type alias Percent =
    Quantity Float Percentile


float : Float -> Percent
float p =
    Quantity p


zero : Percent
zero =
    float 0


toFloat : Percent -> Float
toFloat (Quantity p) =
    p


toPercentage : Percent -> Float
toPercentage (Quantity p) =
    p * 100


toMultiplier : Percent -> Float
toMultiplier (Quantity p) =
    1 + p


capAtHundred : Percent -> Percent
capAtHundred (Quantity p) =
    Quantity (min 100 p)


increaseByPercent : Percent -> Float -> Float
increaseByPercent (Quantity p) value =
    value + (value * (p / 100))


reduceIntByPercent : Percent -> Int -> Int
reduceIntByPercent (Quantity p) value =
    floor ((1 - p) * Basics.toFloat value)