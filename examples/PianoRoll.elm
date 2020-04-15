module Main exposing (Model, Msg(..), init, subscriptions, update, view)

import Css exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
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
import Maestro.Pitch exposing (Pitch, newPitch, pitchToString)
import Maestro.PitchClass exposing (PitchClass(..), pitchClassToString)
import Maestro.Note exposing (newNote, midi)

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
    Grid.container []         -- Responsive fixed width container
        [ CDN.stylesheet      -- Inlined Bootstrap CSS for use with reactor
        , Html.node "style" [] [ text css ]
        , chordSelector model
        , keyboard model
        ]

keyboard : Model -> Html msg
keyboard model =
    let
        chordNotes = List.map (\p -> newNote p.class p.accidental 3) (inversion model.inversion model.chord)
        selectedMidiNumbers = List.map midi chordNotes
    in
    div [ class "keys" ] 
        [ div [ if List.member 60 selectedMidiNumbers then class "key pressed" else class "key" ] []
        , div [ if List.member 61 selectedMidiNumbers then class "key pressed black black1" else class "key black black1" ] []
        , div [ if List.member 62 selectedMidiNumbers then class "key pressed" else class "key" ] []
        , div [ if List.member 63 selectedMidiNumbers then class "key pressed black black3" else class "key black black3" ] []
        , div [ if List.member 64 selectedMidiNumbers then class "key pressed" else class "key" ] []
        , div [ if List.member 65 selectedMidiNumbers then class "key pressed" else class "key" ] []
        , div [ if List.member 66 selectedMidiNumbers then class "key pressed black black1" else class "key black black1" ] []
        , div [ if List.member 67 selectedMidiNumbers then class "key pressed" else class "key" ] []
        , div [ if List.member 68 selectedMidiNumbers then class "key pressed black black2" else class "key black black2" ] []
        , div [ if List.member 69 selectedMidiNumbers then class "key pressed" else class "key" ] []
        , div [ if List.member 70 selectedMidiNumbers then class "key pressed black black3" else class "key black black3" ] []
        , div [ if List.member 71 selectedMidiNumbers then class "key pressed" else class "key" ] []
        , div [ if List.member 72 selectedMidiNumbers then class "key pressed" else class "key" ] []
        , div [ if List.member 73 selectedMidiNumbers then class "key pressed black black1" else class "key black black1" ] []
        , div [ if List.member 74 selectedMidiNumbers then class "key pressed" else class "key" ] []
        , div [ if List.member 75 selectedMidiNumbers then class "key pressed black black3" else class "key black black3" ] []
        , div [ if List.member 76 selectedMidiNumbers then class "key pressed" else class "key" ] []
        , div [ if List.member 77 selectedMidiNumbers then class "key pressed" else class "key" ] []
        , div [ if List.member 78 selectedMidiNumbers then class "key pressed black black1" else class "key black black1" ] []
        , div [ if List.member 79 selectedMidiNumbers then class "key pressed" else class "key" ] []
        , div [ if List.member 80 selectedMidiNumbers then class "key pressed black black2" else class "key black black2" ] []
        , div [ if List.member 81 selectedMidiNumbers then class "key pressed" else class "key" ] []
        , div [ if List.member 82 selectedMidiNumbers then class "key pressed black black3" else class "key black black3" ] []
        , div [ if List.member 83 selectedMidiNumbers then class "key pressed" else class "key" ] []
        , div [ if List.member 84 selectedMidiNumbers then class "key pressed" else class "key" ] []
        , div [ if List.member 85 selectedMidiNumbers then class "key pressed black black1" else class "key black black1" ] []
        ]

-- key : KeyColor -> Int -> Bool -> Html Msg
-- key c number selected =
--     index = 
--     case c of
--         White -> div [ class "key", attribute "data-key" "C3", attribute "data-midi" "60" ] []
--         Black -> div [ class "key black black1" ] []

type KeyColor
    = White
    | Black

colorOf : Int -> KeyColor
colorOf idx =
    let
        indexInOctave =
            remainderBy 12 idx
    in
        if List.member indexInOctave [ 1, 3, 6, 8, 10 ] then
            Black
        else
            White

chordSelector : Model -> Html Msg
chordSelector model =
    Grid.row []
        [ Grid.col []
            [ div []
                [ Dropdown.dropdown
                    model.myDrop1State
                    { options = []
                    , toggleMsg = MyDrop1Msg
                    , toggleButton =
                        Dropdown.toggle [ Button.primary ] [ text <| pitchClassToString model.chord.root.class ]
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
                        , Dropdown.buttonItem [ onClick <| SetChordTypeMsg DominantSeventh ] [ text <| Maestro.Chord.typeToString DominantSeventh ]
                        , Dropdown.buttonItem [ onClick <| SetChordTypeMsg MinorSeventh ] [ text <| Maestro.Chord.typeToString MinorSeventh ]
                        , Dropdown.buttonItem [ onClick <| SetChordTypeMsg MajorSeventh ] [ text <| Maestro.Chord.typeToString MajorSeventh ]
                        , Dropdown.buttonItem [ onClick <| SetChordTypeMsg DiminishedSeventh ] [ text <| Maestro.Chord.typeToString DiminishedSeventh ]
                        , Dropdown.buttonItem [ onClick <| SetChordTypeMsg HalfDiminishedSeventh ] [ text <| Maestro.Chord.typeToString HalfDiminishedSeventh ]
                        , Dropdown.buttonItem [ onClick <| SetChordTypeMsg DominantNinth ] [ text <| Maestro.Chord.typeToString DominantNinth ]
                        , Dropdown.buttonItem [ onClick <| SetChordTypeMsg DominantEleventh ] [ text <| Maestro.Chord.typeToString DominantEleventh ]
                        , Dropdown.buttonItem [ onClick <| SetChordTypeMsg DominantThirteenth ] [ text <| Maestro.Chord.typeToString DominantThirteenth ]
                        , Dropdown.buttonItem [ onClick <| SetChordTypeMsg SuspendedSecond ] [ text <| Maestro.Chord.typeToString SuspendedSecond ]
                        , Dropdown.buttonItem [ onClick <| SetChordTypeMsg SuspendedFourth ] [ text <| Maestro.Chord.typeToString SuspendedFourth ]
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
        , Grid.col [] [ text <| String.join " " <| List.map pitchToString <| inversion model.inversion model.chord ]
        ]


css : String
css =
    """
body {
    background-color: #ddd;
}

h1 {
    border-bottom: .2em dashed black;
}


#piano .keys {
    /*width: 1144px;*/
    width: 780px;
    padding: 2px 0 0 2px;
    overflow: hidden;
    background: #000;
    border-radius: 0 0 4px 4px;
}

.key {
    float: left;
    position: relative;
    width: 50px;
    height: 180px;
    margin: 0 2px 2px 0;
    background: #fff;
    border-radius: 0 0 4px 4px;
}

.key.pressed,
.key:active {
    /*background: #f4f3f3;*/
    background: #007bff;
    box-shadow: inset 3px 2px 3px #999, inset -3px 2px 3px #999;
}
.key.black {
    width: 0;
    margin: 0;
    z-index: 2;
}
.key.black:after {
    content: "";
    position: absolute;
    top: -2px;
    left: -16px;
    display: block;
    width: 32px;
    height: 117px;
    background: #000;
    border-radius: 0 0 4px 4px;
    box-shadow: 1px 1px 0 #555, 2px 2px 0 #555;
}
.key.black1:after {
    left: -20px;
}
.key.black3:after {
    left: -16px;
}
.key.black.pressed:after,
.key.black:active:after {
    /*background-color: #222;*/
    background-color: #007bff;
}
"""