module Scale exposing (Scale, Mode(..), scale)

import Key exposing (Tone)
import Note exposing (Note, newNote)
import Interval exposing (Interval(..), addInterval, majorIntervals, minorIntervals)


{-|
-}
type alias Scale =
    List Tone


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


scale : Tone -> Mode -> List Tone
scale tone mode =
    let
        placeholderNote =
            newNote tone.key tone.adjustment 3
    in
        List.map (\i -> (addInterval placeholderNote i).tone) (modeToIntervals mode)



-- {-|
-- -}
-- scale : Note -> Mode -> Scale
-- scale note mode =
--     List.map (\i -> addInterval note i) (modeToIntervals mode)
