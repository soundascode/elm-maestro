module Maestro.ChordTests exposing (all)

import Expect
import Maestro.Accidental exposing (Accidental(..))
import Maestro.Chord exposing (Type(..), inversion, newChord, pitches)
import Maestro.Pitch exposing (newPitch)
import Maestro.PitchClass exposing (PitchClass(..))
import Maestro.Quality exposing (Quality(..))
import Test exposing (Test, describe, test)


all : Test
all =
    describe "The Chord Module"
        [ describe "Maestro.Chord.inversion"
            [ test "produces the correct first inversion of a major triad" <|
                \_ ->
                    newChord (newPitch C Natural) MajorTriad
                        |> inversion 1
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Natural
                            , newPitch G Natural
                            ]
            , test "produces the correct second inversion of a major triad" <|
                \_ ->
                    newChord (newPitch C Natural) MajorTriad
                        |> inversion 2
                        |> Expect.equal
                            [ newPitch E Natural
                            , newPitch G Natural
                            , newPitch C Natural
                            ]
            , test "produces the correct third inversion of a major triad" <|
                \_ ->
                    newChord (newPitch C Natural) MajorTriad
                        |> inversion 3
                        |> Expect.equal
                            [ newPitch G Natural
                            , newPitch C Natural
                            , newPitch E Natural
                            ]
            ]
        , describe "Maestro.Chord.pitches"
            [ test "returns the correct pitches for a major triad" <|
                \_ ->
                    newChord (newPitch C Natural) MajorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Natural
                            , newPitch G Natural
                            ]
            , test "returns the correct pitches for a minor triad" <|
                \_ ->
                    newChord (newPitch C Natural) MinorTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Flat
                            , newPitch G Natural
                            ]
            , test "returns the correct pitches for an augmented triad" <|
                \_ ->
                    newChord (newPitch C Natural) AugmentedTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Natural
                            , newPitch G Sharp
                            ]
            , test "returns the correct pitches for a diminished triad" <|
                \_ ->
                    newChord (newPitch C Natural) DiminishedTriad
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Flat
                            , newPitch G Flat
                            ]
            , test "returns the correct pitches for a diminished seventh chord" <|
                \_ ->
                    newChord (newPitch C Natural) DiminishedSeventh
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Flat
                            , newPitch G Flat
                            , newPitch B FlatFlat
                            ]
            , test "returns the correct pitches for a half-diminished seventh chord" <|
                \_ ->
                    newChord (newPitch C Natural) HalfDiminishedSeventh
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Flat
                            , newPitch G Flat
                            , newPitch B Flat
                            ]
            , test "returns the correct pitches for a minor seventh chord" <|
                \_ ->
                    newChord (newPitch C Natural) MinorSeventh
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Flat
                            , newPitch G Natural
                            , newPitch B Flat
                            ]
            , test "returns the correct pitches for a minor-major seventh chord" <|
                \_ ->
                    newChord (newPitch C Natural) MinorMajorSeventh
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Flat
                            , newPitch G Natural
                            , newPitch B Natural
                            ]
            , test "returns the correct pitches for a dominant seventh chord" <|
                \_ ->
                    newChord (newPitch C Natural) DominantSeventh
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Natural
                            , newPitch G Natural
                            , newPitch B Flat
                            ]
            , test "returns the correct pitches for a major seventh chord" <|
                \_ ->
                    newChord (newPitch C Natural) MajorSeventh
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Natural
                            , newPitch G Natural
                            , newPitch B Natural
                            ]
            , test "returns the correct pitches for an augmented seventh chord" <|
                \_ ->
                    newChord (newPitch C Natural) AugmentedSeventh
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Natural
                            , newPitch G Sharp
                            , newPitch B Flat
                            ]
            , test "returns the correct pitches for an augmented major seventh chord" <|
                \_ ->
                    newChord (newPitch C Natural) AugmentedMajorSeventh
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Natural
                            , newPitch G Sharp
                            , newPitch B Natural
                            ]
            , test "returns the correct pitches for a dominant ninth chord" <|
                \_ ->
                    newChord (newPitch C Natural) DominantNinth
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Natural
                            , newPitch G Natural
                            , newPitch B Flat
                            , newPitch D Natural
                            ]
            , test "returns the correct pitches for a dominant eleventh chord" <|
                \_ ->
                    newChord (newPitch C Natural) DominantEleventh
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch G Natural
                            , newPitch B Flat
                            , newPitch D Natural
                            , newPitch F Natural
                            ]
            , test "returns the correct pitches for a dominant thirteenth chord" <|
                \_ ->
                    newChord (newPitch C Natural) DominantThirteenth
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Natural
                            , newPitch G Natural
                            , newPitch B Flat
                            , newPitch D Natural
                            , newPitch A Natural
                            ]
            , test "returns the correct pitches for a seventh augmented fifth chord" <|
                \_ ->
                    newChord (newPitch C Natural) SeventhAugmentedFifth
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Natural
                            , newPitch G Sharp
                            , newPitch B Flat
                            ]
            , test "returns the correct pitches for a seventh minor ninth chord" <|
                \_ ->
                    newChord (newPitch C Natural) SeventhMinorNinth
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Natural
                            , newPitch G Natural
                            , newPitch B Flat
                            , newPitch D Flat
                            ]
            , test "returns the correct pitches for a seventh sharp ninth chord" <|
                \_ ->
                    newChord (newPitch C Natural) SeventhSharpNinth
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Natural
                            , newPitch G Natural
                            , newPitch B Flat
                            , newPitch D Sharp
                            ]
            , test "returns the correct pitches for a seventh augmented eleventh chord" <|
                \_ ->
                    newChord (newPitch C Natural) SeventhAugmentedEleventh
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Natural
                            , newPitch G Natural
                            , newPitch B Flat
                            , newPitch D Natural
                            , newPitch F Sharp
                            ]
            , test "returns the correct pitches for a seventh diminished thirteenth chord" <|
                \_ ->
                    newChord (newPitch C Natural) SeventhDiminishedThirteenth
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Natural
                            , newPitch G Natural
                            , newPitch B Flat
                            , newPitch D Natural
                            , newPitch F Natural
                            , newPitch A Flat
                            ]
            , test "returns the correct pitches for an add ninth chord" <|
                \_ ->
                    newChord (newPitch C Natural) AddNinth
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Natural
                            , newPitch G Natural
                            , newPitch D Natural
                            ]
            , test "returns the correct pitches for an add fourth chord" <|
                \_ ->
                    newChord (newPitch C Natural) AddFourth
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Natural
                            , newPitch G Natural
                            , newPitch F Natural
                            ]
            , test "returns the correct pitches for an add sixth chord" <|
                \_ ->
                    newChord (newPitch C Natural) AddSixth
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Natural
                            , newPitch G Natural
                            , newPitch A Natural
                            ]
            , test "returns the correct pitches for a six nine chord" <|
                \_ ->
                    newChord (newPitch C Natural) SixNine
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Natural
                            , newPitch G Natural
                            , newPitch A Natural
                            , newPitch D Natural
                            ]
            , test "returns the correct pitches for a seven six chord" <|
                \_ ->
                    newChord (newPitch C Natural) SevenSix
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Natural
                            , newPitch G Natural
                            , newPitch A Natural
                            , newPitch B Flat
                            ]
            , test "returns the correct pitches for a mixed third chord" <|
                \_ ->
                    newChord (newPitch C Natural) MixedThird
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch E Flat
                            , newPitch E Natural
                            , newPitch G Natural
                            ]
            , test "returns the correct pitches for a sus2 chord" <|
                \_ ->
                    newChord (newPitch C Natural) SuspendedSecond
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch D Natural
                            , newPitch G Natural
                            ]
            , test "returns the correct pitches for a sus4 chord" <|
                \_ ->
                    newChord (newPitch C Natural) SuspendedFourth
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch F Natural
                            , newPitch G Natural
                            ]
            , test "returns the correct pitches for a jazz sus chord" <|
                \_ ->
                    newChord (newPitch C Natural) JazzSus
                        |> pitches
                        |> Expect.equal
                            [ newPitch C Natural
                            , newPitch F Natural
                            , newPitch G Natural
                            , newPitch B Flat
                            , newPitch D Natural
                            ]
            ]
        ]
