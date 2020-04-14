module Maestro.NoteTests exposing (..)

import Expect
import Maestro.Accidental exposing (Accidental(..))
import Maestro.Note exposing (midi, newNote)
import Maestro.PitchClass exposing (PitchClass(..))
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Note test Suite"
        [ describe "notes to midi number tests"
            [ test "correct midi number for C0" <|
                \_ ->
                    newNote C Natural -2
                        |> midi
                        |> Expect.equal 0
            , test "correct midi number for C#0" <|
                \_ ->
                    newNote C Sharp -2
                        |> midi
                        |> Expect.equal 1
            , test "correct midi number for  Db0" <|
                \_ ->
                    newNote D Flat -2
                        |> midi
                        |> Expect.equal 1
            , test "correct midi number for B2" <|
                \_ ->
                    newNote B Natural 2
                        |> midi
                        |> Expect.equal 59
            , test "correct midi number for C3" <|
                \_ ->
                    newNote C Natural 3
                        |> midi
                        |> Expect.equal 60
            , test "correct midi number for C#3" <|
                \_ ->
                    newNote C Sharp 3
                        |> midi
                        |> Expect.equal 61
            , test "correct midi number for Db3" <|
                \_ ->
                    newNote D Flat 3
                        |> midi
                        |> Expect.equal 61
            ]
        ]
