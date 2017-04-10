module Tests exposing (..)

import Test exposing (..)
import MaestroTests exposing (all)
import IntervalTests exposing (all)
import ScaleTests exposing (all)
import ChordTests exposing (all)


all : Test
all =
    describe "elm-maestro"
        [ MaestroTests.all
        , IntervalTests.all
        , ScaleTests.all
        , ChordTests.all
        ]
