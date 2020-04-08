module Interval exposing (all)

import Expect
import Maestro.Accidental exposing (Accidental(..))
import Maestro.Interval exposing (Degree(..), Interval(..), addInterval, diatonicDegreeOf, distance)
import Maestro.Note exposing (newNote)
import Maestro.PitchClass exposing (PitchClass(..))
import Test exposing (Test, describe, test)


all : Test
all =
    describe "MusicTheory Test Suite"
        [ describe "addInterval tests"
            [ test "from Natural to Natural" <|
                \() -> Expect.equal (addInterval (newNote C Natural 3) MajorSeventh) (newNote B Natural 3)
            , test "from Natural to Flat" <|
                \() -> Expect.equal (addInterval (newNote C Natural 3) MinorSeventh) (newNote B Flat 3)
            , test "from Natural to Sharp" <|
                \() -> Expect.equal (addInterval (newNote E Natural 3) MajorSecond) (newNote F Sharp 3)
            , test "from Sharp to Natural" <|
                \() -> Expect.equal (addInterval (newNote F Sharp 3) MinorThird) (newNote A Natural 3)
            , test "from Sharp to Sharp" <|
                \() -> Expect.equal (addInterval (newNote F Sharp 3) PerfectFifth) (newNote C Sharp 4)
            , test "from Flat to Natural" <|
                \() -> Expect.equal (addInterval (newNote G Flat 3) MinorThird) (newNote B FlatFlat 3)
            , test "from Flat to Flat" <|
                \() -> Expect.equal (addInterval (newNote D Flat 3) MinorThird) (newNote F Flat 3)
            , test "octave overflow" <|
                \() -> Expect.equal (addInterval (newNote B Natural 3) MinorThird) (newNote D Natural 4)
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
