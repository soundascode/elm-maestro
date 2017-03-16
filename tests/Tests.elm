module Tests exposing (..)

import Degree exposing (Degree(..), substractDegree)
import Interval exposing (Interval(..))
import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import MusicTheoryTests exposing (all)
import DegreeTests exposing (all)


all : Test
all =
    describe "elm-maestro"
        [ MusicTheoryTests.all
        , DegreeTests.all
        ]
