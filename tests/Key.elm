module Key exposing (all)

import Expect
import Maestro.Accidental exposing (Accidental(..))
import Maestro.Chord exposing (newChord)
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
                            [ newChord (newPitch C Natural) Major
                            , newChord (newPitch D Natural) Minor
                            , newChord (newPitch E Natural) Minor
                            , newChord (newPitch F Natural) Major
                            , newChord (newPitch G Natural) Major
                            , newChord (newPitch A Natural) Minor
                            , newChord (newPitch B Natural) Diminished
                            ]
            , test "chords in the key of D" <|
                \_ ->
                    newKey (newPitch D Natural) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch D Natural) Major
                            , newChord (newPitch E Natural) Minor
                            , newChord (newPitch F Sharp) Minor
                            , newChord (newPitch G Natural) Major
                            , newChord (newPitch A Natural) Major
                            , newChord (newPitch B Natural) Minor
                            , newChord (newPitch C Sharp) Diminished
                            ]
            , test "chords in the key of E" <|
                \_ ->
                    newKey (newPitch E Natural) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch E Natural) Major
                            , newChord (newPitch F Sharp) Minor
                            , newChord (newPitch G Sharp) Minor
                            , newChord (newPitch A Natural) Major
                            , newChord (newPitch B Natural) Major
                            , newChord (newPitch C Sharp) Minor
                            , newChord (newPitch D Sharp) Diminished
                            ]
            , test "chords in the key of F" <|
                \_ ->
                    newKey (newPitch F Natural) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch F Natural) Major
                            , newChord (newPitch G Natural) Minor
                            , newChord (newPitch A Natural) Minor
                            , newChord (newPitch B Flat) Major
                            , newChord (newPitch C Natural) Major
                            , newChord (newPitch D Natural) Minor
                            , newChord (newPitch E Natural) Diminished
                            ]
            , test "chords in the key of G" <|
                \_ ->
                    newKey (newPitch G Natural) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch G Natural) Major
                            , newChord (newPitch A Natural) Minor
                            , newChord (newPitch B Natural) Minor
                            , newChord (newPitch C Natural) Major
                            , newChord (newPitch D Natural) Major
                            , newChord (newPitch E Natural) Minor
                            , newChord (newPitch F Sharp) Diminished
                            ]
            , test "chords in the key of A" <|
                \_ ->
                    newKey (newPitch A Natural) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch A Natural) Major
                            , newChord (newPitch B Natural) Minor
                            , newChord (newPitch C Sharp) Minor
                            , newChord (newPitch D Natural) Major
                            , newChord (newPitch E Natural) Major
                            , newChord (newPitch F Sharp) Minor
                            , newChord (newPitch G Sharp) Diminished
                            ]
            , test "chords in the key of B" <|
                \_ ->
                    newKey (newPitch B Natural) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch B Natural) Major
                            , newChord (newPitch C Sharp) Minor
                            , newChord (newPitch D Sharp) Minor
                            , newChord (newPitch E Natural) Major
                            , newChord (newPitch F Sharp) Major
                            , newChord (newPitch G Sharp) Minor
                            , newChord (newPitch A Sharp) Diminished
                            ]
            , test "chords in the key of C#" <|
                \_ ->
                    newKey (newPitch C Sharp) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch C Sharp) Major
                            , newChord (newPitch D Sharp) Minor
                            , newChord (newPitch E Sharp) Minor
                            , newChord (newPitch F Sharp) Major
                            , newChord (newPitch G Sharp) Major
                            , newChord (newPitch A Sharp) Minor
                            , newChord (newPitch B Sharp) Diminished
                            ]
            , test "chords in the key of Db" <|
                \_ ->
                    newKey (newPitch D Flat) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch D Flat) Major
                            , newChord (newPitch E Flat) Minor
                            , newChord (newPitch F Natural) Minor
                            , newChord (newPitch G Flat) Major
                            , newChord (newPitch A Flat) Major
                            , newChord (newPitch B Flat) Minor
                            , newChord (newPitch C Natural) Diminished
                            ]
            , test "chords in the key of Eb" <|
                \_ ->
                    newKey (newPitch E Flat) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch E Flat) Major
                            , newChord (newPitch F Natural) Minor
                            , newChord (newPitch G Natural) Minor
                            , newChord (newPitch A Flat) Major
                            , newChord (newPitch B Flat) Major
                            , newChord (newPitch C Natural) Minor
                            , newChord (newPitch D Natural) Diminished
                            ]
            , test "chords in the key of F#" <|
                \_ ->
                    newKey (newPitch F Sharp) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch F Sharp) Major
                            , newChord (newPitch G Sharp) Minor
                            , newChord (newPitch A Sharp) Minor
                            , newChord (newPitch B Natural) Major
                            , newChord (newPitch C Sharp) Major
                            , newChord (newPitch D Sharp) Minor
                            , newChord (newPitch E Sharp) Diminished
                            ]
            , test "chords in the key of Gb" <|
                \_ ->
                    newKey (newPitch G Flat) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch G Flat) Major
                            , newChord (newPitch A Flat) Minor
                            , newChord (newPitch B Flat) Minor
                            , newChord (newPitch C Flat) Major
                            , newChord (newPitch D Flat) Major
                            , newChord (newPitch E Flat) Minor
                            , newChord (newPitch F Natural) Diminished
                            ]
            , test "chords in the key of G#" <|
                \_ ->
                    newKey (newPitch G Sharp) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch G Sharp) Major
                            , newChord (newPitch A Sharp) Minor
                            , newChord (newPitch B Sharp) Minor
                            , newChord (newPitch C Sharp) Major
                            , newChord (newPitch D Sharp) Major
                            , newChord (newPitch E Sharp) Minor
                            , newChord (newPitch F SharpSharp) Diminished
                            ]
            , test "chords in the key of Ab" <|
                \_ ->
                    newKey (newPitch A Flat) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch A Flat) Major
                            , newChord (newPitch B Flat) Minor
                            , newChord (newPitch C Natural) Minor
                            , newChord (newPitch D Flat) Major
                            , newChord (newPitch E Flat) Major
                            , newChord (newPitch F Natural) Minor
                            , newChord (newPitch G Natural) Diminished
                            ]
            , test "chords in the key of Bb" <|
                \_ ->
                    newKey (newPitch B Flat) Ionian
                        |> chords
                        |> Expect.equal
                            [ newChord (newPitch B Flat) Major
                            , newChord (newPitch C Natural) Minor
                            , newChord (newPitch D Natural) Minor
                            , newChord (newPitch E Flat) Major
                            , newChord (newPitch F Natural) Major
                            , newChord (newPitch G Natural) Minor
                            , newChord (newPitch A Natural) Diminished
                            ]
            ]
        ]
