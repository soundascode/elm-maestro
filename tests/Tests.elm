module Tests exposing (..)

import MusicTheory exposing (Key(..), Adjustment(..), diatonicDegreeOf, distance)
import Degree exposing (Degree(..))
import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String


all : Test
all =
    describe "Sample Test Suite"
        [ describe "Unit test examples"
            [ test "diatonicDegreeOf in current octave range" <|
                \() ->
                    Expect.equal (diatonicDegreeOf Second C) (Just D)
            , test "diatonicDegreeOf overflow over next octave" <|
                \() ->
                    Expect.equal (diatonicDegreeOf Seventh B) (Just A)
            , test "diatonicDegreeOf octave" <|
                \() ->
                    Expect.equal (diatonicDegreeOf Octave C) (Just C)
            , test "distance between two natural notes" <|
                \() ->
                    Expect.equal (distance { key = C, adjustment = Natural, octave = 3 } { key = D, adjustment = Natural, octave = 3 }) 2
            , test "distance between a natural note and a sharp" <|
                \() ->
                    Expect.equal (distance { key = C, adjustment = Natural, octave = 3 } { key = D, adjustment = Sharp, octave = 3 }) 3
            , test "distance between a natural note and a flat" <|
                \() ->
                    Expect.equal (distance { key = C, adjustment = Natural, octave = 3 } { key = D, adjustment = Flat, octave = 3 }) 1
            , test "distance between a sharp note and a natural" <|
                \() ->
                    Expect.equal (distance { key = C, adjustment = Sharp, octave = 3 } { key = D, adjustment = Natural, octave = 3 }) 1
            , test "distance between a flat note and a natural" <|
                \() ->
                    Expect.equal (distance { key = C, adjustment = Flat, octave = 3 } { key = D, adjustment = Natural, octave = 3 }) 3
            ]
        ]
