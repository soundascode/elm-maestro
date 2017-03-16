module MusicTheoryTests exposing (all)

import MusicTheory exposing (Note, diatonicDegreeOf, distance, addInterval)
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
                \() -> Expect.equal (addInterval { key = C, adjustment = Natural, octave = 3 } MajorSeventh) { key = B, adjustment = Natural, octave = 3 }
            , test "addInterval" <|
                \() -> Expect.equal (addInterval { key = C, adjustment = Natural, octave = 3 } MinorSeventh) { key = B, adjustment = Flat, octave = 3 }
            , test "addInterval" <|
                \() -> Expect.equal (addInterval { key = F, adjustment = Sharp, octave = 3 } PerfectFifth) { key = C, adjustment = Sharp, octave = 4 }
            , test "addInterval" <|
                \() -> Expect.equal (addInterval { key = D, adjustment = Flat, octave = 3 } MinorThird) { key = F, adjustment = Flat, octave = 3 }
            ]
        , describe "distance tests"
            [ test "distance between two natural notes" <|
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
        , describe "diatonicDegreeOf tests"
            [ test "diatonicDegreeOf in current octave range" <|
                \() ->
                    Expect.equal (diatonicDegreeOf Second { key = C, adjustment = Natural, octave = 3 }) ({ key = D, adjustment = Natural, octave = 3 })
            , test "diatonicDegreeOf overflow over next octave" <|
                \() ->
                    Expect.equal (diatonicDegreeOf Seventh { key = B, adjustment = Natural, octave = 3 }) ({ key = A, adjustment = Natural, octave = 4 })
            , test "diatonicDegreeOf octave" <|
                \() ->
                    Expect.equal (diatonicDegreeOf Octave { key = C, adjustment = Natural, octave = 3 }) ({ key = C, adjustment = Natural, octave = 4 })
            ]
        ]
