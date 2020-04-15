module Main exposing (Model, Msg(..), init, subscriptions, update, view)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Grid as Grid
import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maestro.Accidental exposing (Accidental(..))
import Maestro.Chord exposing (..)
import Maestro.Pitch exposing (Pitch, newPitch, toString)
import Maestro.PitchClass exposing (PitchClass(..), toString)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- Dropdowns depends on view state to keep track of whether it is (/should be) open or not


type alias Model =
    { chord : Chord
    , inversion : Int
    , myDrop1State : Dropdown.State
    , myDrop2State : Dropdown.State
    }



-- init


init : () -> ( Model, Cmd Msg )
init _ =
    ( { chord = newChord (newPitch C Natural) MajorTriad
      , inversion = 1
      , myDrop1State = Dropdown.initialState
      , myDrop2State = Dropdown.initialState
      }
    , Cmd.none
    )



-- Msg


type Msg
    = MyDrop1Msg Dropdown.State
    | MyDrop2Msg Dropdown.State
    | SetChordRootMsg Pitch
    | SetChordTypeMsg Maestro.Chord.Type
    | SetInversionMsg Int



-- In your update function you will to handle messages coming from the dropdown


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        MyDrop1Msg state ->
            ( { model | myDrop1State = state }
            , Cmd.none
            )

        MyDrop2Msg state ->
            ( { model | myDrop2State = state }
            , Cmd.none
            )

        SetChordRootMsg pitch ->
            ( { model | chord = { root = pitch, chordType = model.chord.chordType } }, Cmd.none )

        SetChordTypeMsg t ->
            ( { model | chord = { root = model.chord.root, chordType = t } }, Cmd.none )

        SetInversionMsg inversion ->
            ( { model | inversion = inversion }, Cmd.none )



-- Dropdowns relies on subscriptions to automatically close any open when clicking outside them


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Dropdown.subscriptions model.myDrop1State MyDrop1Msg
        , Dropdown.subscriptions model.myDrop2State MyDrop2Msg
        ]



-- Specify config and how the dropdown should look in your view (or view helper) function


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
        , Grid.row []
            [ Grid.col []
                [ div []
                    [ Dropdown.dropdown
                        model.myDrop1State
                        { options = []
                        , toggleMsg = MyDrop1Msg
                        , toggleButton =
                            Dropdown.toggle [ Button.primary ] [ text <| toString model.chord.root.class ]
                        , items =
                            [ Dropdown.buttonItem [ onClick <| SetChordRootMsg <| newPitch C Natural ] [ text "C" ]
                            , Dropdown.buttonItem [ onClick <| SetChordRootMsg <| newPitch C Sharp ] [ text "C#" ]
                            , Dropdown.buttonItem [ onClick <| SetChordRootMsg <| newPitch D Natural ] [ text "D" ]
                            , Dropdown.buttonItem [ onClick <| SetChordRootMsg <| newPitch D Sharp ] [ text "D#" ]
                            , Dropdown.buttonItem [ onClick <| SetChordRootMsg <| newPitch E Natural ] [ text "E" ]
                            , Dropdown.buttonItem [ onClick <| SetChordRootMsg <| newPitch F Natural ] [ text "F" ]
                            , Dropdown.buttonItem [ onClick <| SetChordRootMsg <| newPitch F Sharp ] [ text "F#" ]
                            , Dropdown.buttonItem [ onClick <| SetChordRootMsg <| newPitch G Natural ] [ text "G" ]
                            , Dropdown.buttonItem [ onClick <| SetChordRootMsg <| newPitch G Sharp ] [ text "G#" ]
                            , Dropdown.buttonItem [ onClick <| SetChordRootMsg <| newPitch A Natural ] [ text "A" ]
                            , Dropdown.buttonItem [ onClick <| SetChordRootMsg <| newPitch A Sharp ] [ text "A#" ]
                            , Dropdown.buttonItem [ onClick <| SetChordRootMsg <| newPitch B Natural ] [ text "B" ]
                            ]
                        }

                    -- etc
                    ]
                ]
            , Grid.col []
                [ div []
                    [ Dropdown.dropdown
                        model.myDrop2State
                        { options = []
                        , toggleMsg = MyDrop2Msg
                        , toggleButton =
                            Dropdown.toggle [ Button.primary ] [ text <| Maestro.Chord.typeToString model.chord.chordType ]
                        , items =
                            [ Dropdown.buttonItem [ onClick <| SetChordTypeMsg MajorTriad ] [ text <| Maestro.Chord.typeToString MajorTriad ]
                            , Dropdown.buttonItem [ onClick <| SetChordTypeMsg MinorTriad ] [ text <| Maestro.Chord.typeToString MinorTriad ]
                            , Dropdown.buttonItem [ onClick <| SetChordTypeMsg AugmentedTriad ] [ text <| Maestro.Chord.typeToString AugmentedTriad ]
                            , Dropdown.buttonItem [ onClick <| SetChordTypeMsg DiminishedTriad ] [ text <| Maestro.Chord.typeToString DiminishedTriad ]
                            , Dropdown.buttonItem [ onClick <| SetChordTypeMsg HalfDiminishedSeventh ] [ text <| Maestro.Chord.typeToString HalfDiminishedSeventh ]
                            , Dropdown.buttonItem [ onClick <| SetChordTypeMsg DominantSeventh ] [ text <| Maestro.Chord.typeToString DominantSeventh ]
                            , Dropdown.buttonItem [ onClick <| SetChordTypeMsg MajorSeventh ] [ text <| Maestro.Chord.typeToString MajorSeventh ]
                            ]
                        }
                    ]
                ]
            , Grid.col []
                [ div []
                    [ Dropdown.dropdown
                        model.myDrop2State
                        { options = []
                        , toggleMsg = MyDrop2Msg
                        , toggleButton =
                            Dropdown.toggle [ Button.primary ] [ text "Pick an inversion" ]
                        , items =
                            [ Dropdown.buttonItem [ onClick <| SetInversionMsg 1 ] [ text "First" ]
                            , Dropdown.buttonItem [ onClick <| SetInversionMsg 2 ] [ text "Second" ]
                            , Dropdown.buttonItem [ onClick <| SetInversionMsg 3 ] [ text "Third" ]
                            ]
                        }
                    ]
                ]
            , Grid.col [] [ text <| String.join " " <| List.map toString <| inversion model.inversion model.chord ]
            ]
        ]
