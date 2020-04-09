module Key exposing (all)

import Expect
import Maestro.Accidental exposing (Accidental(..))
import Maestro.Chord exposing (Type(..), newChord)
import Maestro.Key exposing (Key, chords, newKey)
import Maestro.Pitch exposing (newPitch)
import Maestro.PitchClass exposing (PitchClass(..))
import Maestro.Quality exposing (Quality(..))
import Maestro.Scale exposing (Scale(..))
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Chord Test Suite"
        [ describe "Inversions tests"
            [ test "chords in the key of C" <|
                \_ ->
                    newKey (newPitch C Natural) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch C Natural) MajorTriad
                            , newChord (newPitch D Natural) MinorTriad
                            , newChord (newPitch E Natural) MinorTriad
                            , newChord (newPitch F Natural) MajorTriad
                            , newChord (newPitch G Natural) MajorTriad
                            , newChord (newPitch A Natural) MinorTriad
                            , newChord (newPitch B Natural) DiminishedTriad
                            ]
            , test "chords in the key of D" <|
                \_ ->
                    newKey (newPitch D Natural) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch D Natural) MajorTriad
                            , newChord (newPitch E Natural) MinorTriad
                            , newChord (newPitch F Sharp) MinorTriad
                            , newChord (newPitch G Natural) MajorTriad
                            , newChord (newPitch A Natural) MajorTriad
                            , newChord (newPitch B Natural) MinorTriad
                            , newChord (newPitch C Sharp) DiminishedTriad
                            ]
            , test "chords in the key of E" <|
                \_ ->
                    newKey (newPitch E Natural) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch E Natural) MajorTriad
                            , newChord (newPitch F Sharp) MinorTriad
                            , newChord (newPitch G Sharp) MinorTriad
                            , newChord (newPitch A Natural) MajorTriad
                            , newChord (newPitch B Natural) MajorTriad
                            , newChord (newPitch C Sharp) MinorTriad
                            , newChord (newPitch D Sharp) DiminishedTriad
                            ]
            , test "chords in the key of F" <|
                \_ ->
                    newKey (newPitch F Natural) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch F Natural) MajorTriad
                            , newChord (newPitch G Natural) MinorTriad
                            , newChord (newPitch A Natural) MinorTriad
                            , newChord (newPitch B Flat) MajorTriad
                            , newChord (newPitch C Natural) MajorTriad
                            , newChord (newPitch D Natural) MinorTriad
                            , newChord (newPitch E Natural) DiminishedTriad
                            ]
            , test "chords in the key of G" <|
                \_ ->
                    newKey (newPitch G Natural) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch G Natural) MajorTriad
                            , newChord (newPitch A Natural) MinorTriad
                            , newChord (newPitch B Natural) MinorTriad
                            , newChord (newPitch C Natural) MajorTriad
                            , newChord (newPitch D Natural) MajorTriad
                            , newChord (newPitch E Natural) MinorTriad
                            , newChord (newPitch F Sharp) DiminishedTriad
                            ]
            , test "chords in the key of A" <|
                \_ ->
                    newKey (newPitch A Natural) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch A Natural) MajorTriad
                            , newChord (newPitch B Natural) MinorTriad
                            , newChord (newPitch C Sharp) MinorTriad
                            , newChord (newPitch D Natural) MajorTriad
                            , newChord (newPitch E Natural) MajorTriad
                            , newChord (newPitch F Sharp) MinorTriad
                            , newChord (newPitch G Sharp) DiminishedTriad
                            ]
            , test "chords in the key of B" <|
                \_ ->
                    newKey (newPitch B Natural) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch B Natural) MajorTriad
                            , newChord (newPitch C Sharp) MinorTriad
                            , newChord (newPitch D Sharp) MinorTriad
                            , newChord (newPitch E Natural) MajorTriad
                            , newChord (newPitch F Sharp) MajorTriad
                            , newChord (newPitch G Sharp) MinorTriad
                            , newChord (newPitch A Sharp) DiminishedTriad
                            ]
            , test "chords in the key of C#" <|
                \_ ->
                    newKey (newPitch C Sharp) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch C Sharp) MajorTriad
                            , newChord (newPitch D Sharp) MinorTriad
                            , newChord (newPitch E Sharp) MinorTriad
                            , newChord (newPitch F Sharp) MajorTriad
                            , newChord (newPitch G Sharp) MajorTriad
                            , newChord (newPitch A Sharp) MinorTriad
                            , newChord (newPitch B Sharp) DiminishedTriad
                            ]
            , test "chords in the key of Db" <|
                \_ ->
                    newKey (newPitch D Flat) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch D Flat) MajorTriad
                            , newChord (newPitch E Flat) MinorTriad
                            , newChord (newPitch F Natural) MinorTriad
                            , newChord (newPitch G Flat) MajorTriad
                            , newChord (newPitch A Flat) MajorTriad
                            , newChord (newPitch B Flat) MinorTriad
                            , newChord (newPitch C Natural) DiminishedTriad
                            ]
            , test "chords in the key of Eb" <|
                \_ ->
                    newKey (newPitch E Flat) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch E Flat) MajorTriad
                            , newChord (newPitch F Natural) MinorTriad
                            , newChord (newPitch G Natural) MinorTriad
                            , newChord (newPitch A Flat) MajorTriad
                            , newChord (newPitch B Flat) MajorTriad
                            , newChord (newPitch C Natural) MinorTriad
                            , newChord (newPitch D Natural) DiminishedTriad
                            ]
            , test "chords in the key of F#" <|
                \_ ->
                    newKey (newPitch F Sharp) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch F Sharp) MajorTriad
                            , newChord (newPitch G Sharp) MinorTriad
                            , newChord (newPitch A Sharp) MinorTriad
                            , newChord (newPitch B Natural) MajorTriad
                            , newChord (newPitch C Sharp) MajorTriad
                            , newChord (newPitch D Sharp) MinorTriad
                            , newChord (newPitch E Sharp) DiminishedTriad
                            ]
            , test "chords in the key of Gb" <|
                \_ ->
                    newKey (newPitch G Flat) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch G Flat) MajorTriad
                            , newChord (newPitch A Flat) MinorTriad
                            , newChord (newPitch B Flat) MinorTriad
                            , newChord (newPitch C Flat) MajorTriad
                            , newChord (newPitch D Flat) MajorTriad
                            , newChord (newPitch E Flat) MinorTriad
                            , newChord (newPitch F Natural) DiminishedTriad
                            ]
            , test "chords in the key of G#" <|
                \_ ->
                    newKey (newPitch G Sharp) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch G Sharp) MajorTriad
                            , newChord (newPitch A Sharp) MinorTriad
                            , newChord (newPitch B Sharp) MinorTriad
                            , newChord (newPitch C Sharp) MajorTriad
                            , newChord (newPitch D Sharp) MajorTriad
                            , newChord (newPitch E Sharp) MinorTriad
                            , newChord (newPitch F SharpSharp) DiminishedTriad
                            ]
            , test "chords in the key of Ab" <|
                \_ ->
                    newKey (newPitch A Flat) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch A Flat) MajorTriad
                            , newChord (newPitch B Flat) MinorTriad
                            , newChord (newPitch C Natural) MinorTriad
                            , newChord (newPitch D Flat) MajorTriad
                            , newChord (newPitch E Flat) MajorTriad
                            , newChord (newPitch F Natural) MinorTriad
                            , newChord (newPitch G Natural) DiminishedTriad
                            ]
            , test "chords in the key of Bb" <|
                \_ ->
                    newKey (newPitch B Flat) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch B Flat) MajorTriad
                            , newChord (newPitch C Natural) MinorTriad
                            , newChord (newPitch D Natural) MinorTriad
                            , newChord (newPitch E Flat) MajorTriad
                            , newChord (newPitch F Natural) MajorTriad
                            , newChord (newPitch G Natural) MinorTriad
                            , newChord (newPitch A Natural) DiminishedTriad
                            ]
            ]
        ]
