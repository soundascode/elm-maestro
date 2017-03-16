module DegreeTests exposing (..)

import MusicTheory exposing (Note, diatonicDegreeOf, distance, addInterval)
import Key exposing (Key(..), Adjustment(..))
import Degree exposing (Degree(..), substractDegree)
import Interval exposing (Interval(..))
import Test exposing (..)
import Expect
import String


all : Test
all =
    describe "Degree Test Suite"
        [ describe "substractDegree tests"
            [ test "in current octave" <|
                \() ->
                    Expect.equal (substractDegree Seventh 1) Sixth
            , test "substractDegree canceling operation" <|
                \() -> Expect.equal (substractDegree First 1) First
            , test "substractDegree underflow" <|
                \() -> Expect.equal (substractDegree First 2) First
            ]
        ]
