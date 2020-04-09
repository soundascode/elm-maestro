module Maestro.Key exposing (Key, chords, newKey)

import Maestro.Chord exposing (Chord, newChord)
import Maestro.Function exposing (Function(..))
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
    , function Dominant k
    , function Submediant k
    , function LeadingTone k
    ]


function : Function -> Key -> Chord
function f k =
    case f of
        Tonic ->
            newChord { class = k.class, accidental = k.accidental } MajorTriad

        Supertonic ->
            newChord (addInterval { class = k.class, accidental = k.accidental } MajorSecond) MinorTriad

        Mediant ->
            newChord (addInterval { class = k.class, accidental = k.accidental } MajorThird) MinorTriad

        Subdominant ->
            newChord (addInterval { class = k.class, accidental = k.accidental } PerfectFourth) MajorTriad

        Dominant ->
            newChord (addInterval { class = k.class, accidental = k.accidental } PerfectFifth) MajorTriad

        Submediant ->
            newChord (addInterval { class = k.class, accidental = k.accidental } MajorSixth) MinorTriad

        LeadingTone ->
            newChord (addInterval { class = k.class, accidental = k.accidental } Maestro.Interval.MajorSeventh) DiminishedTriad


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
