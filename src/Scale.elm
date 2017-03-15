module Scale exposing (Mode(..), scale)

{-|
-}


{-|
-}
type alias Scale =
    List Int


{-|
-}
type Mode
    = Major
    | Minor


{-|
-}
modeToIntervals : Mode -> List Interval
modeToIntervals mode =
    case mode of
        Major ->
            majorIntervals

        Minor ->
            minorIntervals


{-|
-}
scale : Note -> Mode -> Scale
scale note mode =
    List.map (\i -> (noteToIndex note) + intervalToValue i) (modeToIntervals mode)
