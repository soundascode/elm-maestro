module MusicTheoryTests exposing (all)

import MusicTheory exposing (Note, newNote, diatonicDegreeOf, distance, addInterval)
import Key exposing (Key(..), Adjustment(..))
import Degree exposing (Degree(..), substractDegree)
import Interval exposing (Interval(..))
import Test exposing (..)
import Expect
import String


all : Test
all =
    describe "MusicTheory Test Suite"
        [ describe "addInterval tests"
            [ test "addInterval" <|
                \() -> Expect.equal (addInterval (newNote C Natural 3) MajorSeventh) (newNote B Natural 3)
            , test "addInterval" <|
                \() -> Expect.equal (addInterval (newNote C Natural 3) MinorSeventh) (newNote B Flat 3)
            , test "addInterval" <|
                \() -> Expect.equal (addInterval (newNote F Sharp 3) PerfectFifth) (newNote C Sharp 4)
            , test "addInterval" <|
                \() -> Expect.equal (addInterval (newNote D Flat 3) MinorThird) (newNote F Flat 3)
            ]
        , describe "distance tests"
            [ test "distance between two natural notes" <|
                \() ->
                    Expect.equal (distance (newNote C Natural 3) (newNote D Natural 3)) 2
            , test "distance between a natural note and a sharp" <|
                \() ->
                    Expect.equal (distance (newNote C Natural 3) (newNote D Sharp 3)) 3
            , test "distance between a natural note and a flat" <|
                \() ->
                    Expect.equal (distance (newNote C Natural 3) (newNote D Flat 3)) 1
            , test "distance between a sharp note and a natural" <|
                \() ->
                    Expect.equal (distance (newNote C Sharp 3) (newNote D Natural 3)) 1
            , test "distance between a flat note and a natural" <|
                \() ->
                    Expect.equal (distance (newNote C Flat 3) (newNote D Natural 3)) 3
            ]
        , describe "diatonicDegreeOf tests"
            [ test "diatonicDegreeOf in current octave range" <|
                \() ->
                    Expect.equal (diatonicDegreeOf Second (newNote C Natural 3)) (newNote D Natural 3)
            , test "diatonicDegreeOf overflow over next octave" <|
                \() ->
                    Expect.equal (diatonicDegreeOf Seventh (newNote B Natural 3)) (newNote A Natural 4)
            , test "diatonicDegreeOf octave" <|
                \() ->
                    Expect.equal (diatonicDegreeOf Octave (newNote C Natural 3)) (newNote C Natural 4)
            ]
        ]
