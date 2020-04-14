module Maestro.Key exposing (Key, chords, extendedChords, newKey)

import Maestro.Chord exposing (Chord, Type(..), newChord)
import Maestro.Function exposing (Function(..))
import Maestro.Interval exposing (Interval(..), addInterval)
import Maestro.Pitch exposing (Pitch, newPitch)
import Maestro.Quality exposing (Quality(..))
import Maestro.Scale exposing (Scale(..))


type alias Key = Pitch


newKey : Pitch  -> Key
newKey p = p



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

extendedChords : Key -> List Chord
extendedChords k =
    [ functionWithExtension Tonic k
    , functionWithExtension Supertonic k
    , functionWithExtension Mediant k
    , functionWithExtension Subdominant k
    , functionWithExtension Maestro.Function.Dominant k
    , functionWithExtension Submediant k
    , functionWithExtension LeadingTone k
    ]


function : Function -> Key -> Chord
function f k =
    case f of
        Tonic ->
            newChord k MajorTriad

        Supertonic ->
            newChord (addInterval k MajorSecond) MinorTriad

        Mediant ->
            newChord (addInterval k MajorThird) MinorTriad

        Subdominant ->
            newChord (addInterval k PerfectFourth) MajorTriad

        Maestro.Function.Dominant ->
            newChord (addInterval k PerfectFifth) MajorTriad

        Submediant ->
            newChord (addInterval k MajorSixth) MinorTriad

        LeadingTone ->
            newChord (addInterval k Maestro.Interval.MajorSeventh) DiminishedTriad

functionWithExtension : Function -> Key -> Chord
functionWithExtension f k =
    case f of
        Tonic ->
            newChord k Maestro.Chord.MajorSeventh

        Supertonic ->
            newChord (addInterval k MajorSecond) Maestro.Chord.MinorSeventh

        Mediant ->
            newChord (addInterval k MajorThird) Maestro.Chord.MinorSeventh

        Subdominant ->
            newChord (addInterval k PerfectFourth) Maestro.Chord.MajorSeventh

        Maestro.Function.Dominant ->
            newChord (addInterval k PerfectFifth) Maestro.Chord.DominantSeventh

        Submediant ->
            newChord (addInterval k MajorSixth) Maestro.Chord.MinorSeventh

        LeadingTone ->
            newChord (addInterval k Maestro.Interval.MajorSeventh) Maestro.Chord.HalfDiminishedSeventh


{-| addInterval applies an interval to a given note, and returns
the resulting note
-}
addInterval : Key -> Interval -> Key
addInterval k interval =
    let
        note =
            Maestro.Interval.addInterval { pitch = newPitch k.class k.accidental , octave = 3 } interval
    in
        note.pitch
