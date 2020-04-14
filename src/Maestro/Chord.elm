module Maestro.Chord exposing
    ( Chord
    , Type(..), inversion, newChord, pitches, typeToString
    )

{-| This module provides types and functions to create and
manipulate chords.


# Types

@docs Chord, Quality


# Chord generation

@docs chord, inversion1

-}

import Maestro.Interval exposing (Interval(..), addInterval)
import Maestro.ListUtils exposing (rotate)
import Maestro.Note exposing (newNote)
import Maestro.Pitch exposing (Pitch)


type alias Chord =
    { root : Pitch
    , chordType : Type
    }


type Type
    = MajorTriad
    | MinorTriad
    | AugmentedTriad
    | DiminishedTriad
    | DiminishedSeventh
    | HalfDiminishedSeventh
    | MinorSeventh
    | MinorMajorSeventh
    | DominantSeventh
    | MajorSeventh
    | AugmentedSeventh
    | AugmentedMajorSeventh
    | DominantNinth
    | DominantEleventh
    | DominantThirteenth
    | SeventhAugmentedFifth
    | SeventhMinorNinth
    | SeventhSharpNinth
    | SeventhAugmentedEleventh
    | SeventhDiminishedThirteenth
    | AddNinth
    | AddFourth
    | AddSixth
    | SixNine
    | SevenSix
    | MixedThird
    | SuspendedSecond
    | SuspendedFourth
    | JazzSus


newChord : Pitch -> Type -> Chord
newChord p t =
    { root = p, chordType = t }


{-| Given a Chord, returns the pitches composing it.

    (==)
        pitches
        (newChord (newPitch C Natural) Major)
        [ { class = C, accidental = Natural }
        , { class = E, accidental = Natural }
        , { class = G, accidental = Natural }
        ]

-}
pitches : Chord -> List Pitch
pitches c =
    let
        placeholderNote =
            newNote c.root.class c.root.accidental 3
    in
    List.map (\i -> (addInterval placeholderNote i).pitch) (intervalsOf c.chordType)


{-| Chord represents a list of pitches composing it
-}
inversion : Int -> Chord -> List Pitch
inversion i c =
    case i of
        0 ->
            pitches c

        _ ->
            rotate (i - 1) <| pitches c


intervalsOf : Type -> List Interval
intervalsOf t =
    case t of
        MajorTriad ->
            [ PerfectUnison
            , MajorThird
            , PerfectFifth
            ]

        MinorTriad ->
            [ PerfectUnison
            , MinorThird
            , PerfectFifth
            ]

        AugmentedTriad ->
            [ PerfectUnison
            , MajorThird
            , AugmentedFifth
            ]

        DiminishedTriad ->
            [ PerfectUnison
            , MinorThird
            , DiminishedFifth
            ]

        DiminishedSeventh ->
            [ PerfectUnison
            , MinorThird
            , DiminishedFifth
            , Maestro.Interval.DiminishedSeventh
            ]

        HalfDiminishedSeventh ->
            [ PerfectUnison
            , MinorThird
            , DiminishedFifth
            , Maestro.Interval.MinorSeventh
            ]

        MinorSeventh ->
            [ PerfectUnison
            , MinorThird
            , PerfectFifth
            , Maestro.Interval.MinorSeventh
            ]

        MinorMajorSeventh ->
            [ PerfectUnison
            , MinorThird
            , PerfectFifth
            , Maestro.Interval.MajorSeventh
            ]

        DominantSeventh ->
            [ PerfectUnison
            , MajorThird
            , PerfectFifth
            , Maestro.Interval.MinorSeventh
            ]

        MajorSeventh ->
            [ PerfectUnison
            , MajorThird
            , PerfectFifth
            , Maestro.Interval.MajorSeventh
            ]

        AugmentedSeventh ->
            [ PerfectUnison
            , MajorThird
            , AugmentedFifth
            , Maestro.Interval.MinorSeventh
            ]

        AugmentedMajorSeventh ->
            [ PerfectUnison
            , MajorThird
            , AugmentedFifth
            , Maestro.Interval.MajorSeventh
            ]

        DominantNinth ->
            List.concat [ intervalsOf DominantSeventh, [ MajorNinth ] ]

        DominantEleventh ->
            [ PerfectUnison
            , PerfectFifth
            , Maestro.Interval.MinorSeventh
            , MajorNinth
            , PerfectEleventh
            ]

        DominantThirteenth ->
            [ PerfectUnison
            , MajorThird
            , PerfectFifth
            , Maestro.Interval.MinorSeventh
            , MajorNinth
            , MajorThirteenth
            ]

        SeventhAugmentedFifth ->
            [ PerfectUnison
            , MajorThird
            , AugmentedFifth
            , Maestro.Interval.MinorSeventh
            ]

        SeventhMinorNinth ->
            List.concat [ intervalsOf DominantSeventh, [ MinorNinth ] ]

        SeventhSharpNinth ->
            List.concat [ intervalsOf DominantSeventh, [ AugmentedNinth ] ]

        SeventhAugmentedEleventh ->
            List.concat [ intervalsOf DominantSeventh, [ MajorNinth, AugmentedEleventh ] ]

        SeventhDiminishedThirteenth ->
            List.concat [ intervalsOf DominantSeventh, [ MajorNinth, PerfectEleventh, MinorThirteenth ] ]

        AddNinth ->
            List.concat [ intervalsOf MajorTriad, [ MajorNinth ] ]

        AddFourth ->
            List.concat [ intervalsOf MajorTriad, [ PerfectFourth ] ]

        AddSixth ->
            List.concat [ intervalsOf MajorTriad, [ MajorSixth ] ]

        SixNine ->
            List.concat [ intervalsOf MajorTriad, [ MajorSixth, MajorNinth ] ]

        SevenSix ->
            List.concat [ intervalsOf MajorTriad, [ MajorSixth, Maestro.Interval.MinorSeventh ] ]

        MixedThird ->
            [ PerfectUnison
            , MinorThird
            , MajorThird
            , PerfectFifth
            ]

        SuspendedSecond ->
            [ PerfectUnison
            , MajorSecond
            , PerfectFifth
            ]

        SuspendedFourth ->
            [ PerfectUnison
            , PerfectFourth
            , PerfectFifth
            ]

        JazzSus ->
            List.concat [ intervalsOf SuspendedFourth, [ Maestro.Interval.MinorSeventh, MajorNinth ] ]


typeToString : Type -> String
typeToString t =
    case t of
        MajorTriad ->
            "Δ"

        MinorTriad ->
            "m"

        AugmentedTriad ->
            "+"

        DiminishedTriad ->
            "o"

        DiminishedSeventh ->
            "o7"

        HalfDiminishedSeventh ->
            "7♭5"

        MinorSeventh ->
            "m7"

        MinorMajorSeventh ->
            "mM7"

        DominantSeventh ->
            "7"

        MajorSeventh ->
            "maj7"

        AugmentedSeventh ->
            "+7"

        AugmentedMajorSeventh ->
            "+M7"

        DominantNinth ->
            "9"

        DominantEleventh ->
            "11"

        DominantThirteenth ->
            "13"

        SeventhAugmentedFifth ->
            "7+5"

        SeventhMinorNinth ->
            "7-9"

        SeventhSharpNinth ->
            "7+9"

        SeventhAugmentedEleventh ->
            "7+11"

        SeventhDiminishedThirteenth ->
            "7-13"

        AddNinth ->
            "add9"

        AddFourth ->
            "add11"

        AddSixth ->
            "6"

        SixNine ->
            "6/9"

        SevenSix ->
            "7/6"

        MixedThird ->
            ""

        SuspendedSecond ->
            "sus2"

        SuspendedFourth ->
            "sus4"

        JazzSus ->
            "9sus4"
