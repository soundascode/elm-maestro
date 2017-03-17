module Tests exposing (..)

import Test exposing (..)
import MusicTheoryTests exposing (all)
import IntervalTests exposing (all)
import ScaleTests exposing (all)


all : Test
all =
    describe "elm-maestro"
        [ MusicTheoryTests.all
        , IntervalTests.all
        , ScaleTests.all
        ]
