module ScaleTests exposing (all)

import Note exposing (Note)
import Key exposing (Key(..), Adjustment(..), newTone)
import Interval exposing (Interval(..), Degree(..), substractDegree, diatonicDegreeOf, distance, addInterval)
import Scale exposing (Scale, Mode(..), scale)
import Test exposing (..)
import Expect
import String


all : Test
all =
    describe "Scale Test Suite"
        [ describe "Major scale tests"
            [ test "C Major" <|
                \() ->
                    Expect.equal (scale (newTone C Natural) Major)
                        [ newTone C Natural
                        , newTone D Natural
                        , newTone E Natural
                        , newTone F Natural
                        , newTone G Natural
                        , newTone A Natural
                        , newTone B Natural
                        ]
            ]
        ]
