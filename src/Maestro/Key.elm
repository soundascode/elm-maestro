module Maestro.Key exposing (Key, chords, newKey)

import Maestro.Chord exposing (Chord, newChord)
import Maestro.Degree exposing (Degree)
import Maestro.Interval exposing (Interval(..), addInterval)
import Maestro.Pitch exposing (Pitch)
import Maestro.Quality exposing (Quality(..))
import Maestro.Scale exposing (Scale(..))


type alias Key =
    { pitch : Pitch
    , scale : Scale
    }


newKey : Pitch -> Scale -> Key
newKey p s =
    { pitch = p, scale = s }


chords : Key -> List Chord
chords k =
    [ function Tonic k
    , function Supertonic k
    , function Mediant k
    , function Subdominant k
    , function Maestro.Function.Dominant k
    , function Submediant k
    , function LeadingTone k
    ]


function : Function -> Key -> Chord
function f k =
    case f of
        Tonic ->
            newChord k.pitch MajorTriad

        Supertonic ->
            newChord (.pitch <| addInterval k MajorSecond) MinorTriad

        Mediant ->
            newChord (.pitch <| addInterval k MajorThird) MinorTriad

        Subdominant ->
            newChord (.pitch <| addInterval k PerfectFourth) MajorTriad

        Maestro.Function.Dominant ->
            newChord (.pitch <| addInterval k PerfectFifth) MajorTriad

        Submediant ->
            newChord (.pitch <| addInterval k MajorSixth) MinorTriad

        LeadingTone ->
            newChord (.pitch <| addInterval k Maestro.Interval.MajorSeventh) DiminishedTriad


{-| addInterval applies an interval to a given note, and returns
the resulting note
-}
addInterval : Key -> Interval -> Key
addInterval k interval =
    let
        n =
            Maestro.Interval.addInterval { pitch = k.pitch, octave = 3 } interval
    in
        { pitch = n.pitch, scale = k.scale }
