module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Ports exposing (listenForMIDIStatus)



-- {{{ MODEL


type alias Model =
    { midiStatus : MIDIStatus
    , controlState : Dict String ControlState
    , page : Page
    }


type MIDIStatus
    = Initialising
    | FailedToEstablishMIDI
    | MIDIConnected


type alias Page =
    { label : String
    , controller : Controller
    , config : PageConfig
    }


type alias PageConfig =
    { gapSize : Int
    , debug : Bool
    }


createControlState : Page -> Dict String ControlState
createControlState page =
    getControllerIds [] page.controller 0
        |> Dict.fromList


getControllerIds : List String -> Controller -> Int -> List ( String, ControlState )
getControllerIds idParts controller id =
    let
        updatedParts =
            String.fromInt id :: idParts
    in
    case controller of
        Module _ c ->
            getControllerIds updatedParts c 0

        Row controllers ->
            List.map2
                (getControllerIds updatedParts)
                controllers
                (List.range 0 <| List.length controllers)
                |> List.concat

        Column controllers ->
            List.map2
                (getControllerIds updatedParts)
                controllers
                (List.range 0 <| List.length controllers)
                |> List.concat

        Control _ ->
            ( String.join "_" <| List.reverse updatedParts, Default )
                |> List.singleton


type Controller
    = Module String Controller
    | Row (List Controller)
    | Column (List Controller)
    | Control MidiControl


type MidiControl
    = Button


type ControlState
    = Default
    | Pressed


init : ( Model, Cmd Msg )
init =
    ( { midiStatus = Initialising
      , controlState =
            createControlState defaultPage
      , page = defaultPage
      }
    , Cmd.none
    )


defaultPage : Page
defaultPage =
    { label = "1"
    , controller =
        Control Button
            |> List.repeat 8
            |> Row
            |> List.repeat 3
            |> Column
    , config =
        { gapSize = 5
        , debug = True
        }
    }



-- }}}
-- {{{ UPDATE


type Msg
    = MIDIStatusChanged Bool
    | ButtonDown String
    | ButtonUp String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        maybeAlways value =
            Maybe.map (\_ -> value)

        setControlState id newState =
            Dict.update id (maybeAlways newState) model.controlState
    in
    case msg of
        MIDIStatusChanged isConnected ->
            ( if isConnected then
                { model | midiStatus = MIDIConnected }

              else
                { model | midiStatus = FailedToEstablishMIDI }
            , Cmd.none
            )

        ButtonDown id ->
            ( { model
                | controlState =
                    setControlState id Pressed
              }
            , Cmd.none
            )

        ButtonUp id ->
            ( { model
                | controlState =
                    setControlState id Default
              }
            , Cmd.none
            )



-- }}}
-- {{{ VIEW


view : Model -> Element Msg
view model =
    column (padding 5 :: fillSpace)
        [ midiStatus model.midiStatus
        , renderPage model.controlState model.page
        ]


midiStatus : MIDIStatus -> Element Msg
midiStatus status =
    case status of
        Initialising ->
            el [] <| text "Initialising..."

        FailedToEstablishMIDI ->
            el [] <| text "Failed to establish MIDI connection."

        MIDIConnected ->
            el [] <| text "MIDI connection sucessful!"


renderPage : Dict String ControlState -> Page -> Element Msg
renderPage controlStates page =
    let
        { config, controller } =
            page
    in
    el ([ padding config.gapSize, Border.solid, Border.width 2 ] ++ fillSpace) <|
        renderController config controlStates [] controller 0


renderController : PageConfig -> Dict String ControlState -> List String -> Controller -> Int -> Element Msg
renderController config controlStates idParts controller id =
    let
        updatedParts =
            String.fromInt id :: idParts
    in
    case controller of
        Module _ subControls ->
            el
                ([ padding config.gapSize
                 , spacing config.gapSize
                 , Background.color <| rgb255 3 5 5
                 ]
                    ++ fillSpace
                )
                (renderController config controlStates updatedParts subControls 0)

        Row subControls ->
            row
                ([ padding config.gapSize
                 , spacing config.gapSize
                 , Background.color <| rgb255 21 39 200
                 ]
                    ++ fillSpace
                )
            <|
                List.map2
                    (renderController config controlStates updatedParts)
                    subControls
                    (List.range 0 <| List.length subControls)

        Column subControls ->
            column
                ([ padding config.gapSize
                 , spacing config.gapSize
                 , Background.color <| rgb255 21 221 23
                 ]
                    ++ fillSpace
                )
            <|
                List.map2
                    (renderController config controlStates updatedParts)
                    subControls
                    (List.range 0 <| List.length subControls)

        Control midiControl ->
            renderMidiControl
                config
                controlStates
                (updatedParts
                    |> List.reverse
                    |> String.join "_"
                )
                midiControl


renderMidiControl : PageConfig -> Dict String ControlState -> String -> MidiControl -> Element Msg
renderMidiControl config controlStates id midiControl =
    let
        controlState =
            Dict.get id controlStates
                |> Maybe.withDefault Default
    in
    case midiControl of
        Button ->
            el
                ([ padding config.gapSize
                 , spacing config.gapSize
                 , case controlState of
                    Default ->
                        Background.color <| rgb255 221 221 23

                    Pressed ->
                        Background.color <| rgb255 (221 // 2) (221 // 2) (23 // 2)
                 , htmlAttribute <|
                    Touch.onStart
                        (\_ ->
                            ButtonDown id
                        )
                 , htmlAttribute <|
                    Mouse.onDown
                        (\_ ->
                            ButtonDown id
                        )
                 , htmlAttribute <|
                    Touch.onEnd
                        (\_ ->
                            ButtonUp id
                        )
                 , htmlAttribute <|
                    Mouse.onUp
                        (\_ ->
                            ButtonUp id
                        )
                 ]
                    ++ fillSpace
                )
                (if config.debug then
                    case controlState of
                        Default ->
                            text "Off"

                        Pressed ->
                            text "On"

                 else
                    none
                )


fillSpace : List (Attribute msg)
fillSpace =
    [ height fill, width fill ]



-- }}}
-- {{{ PROGRAM


main : Program () Model Msg
main =
    Browser.element
        { view = view >> layout []
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ listenForMIDIStatus MIDIStatusChanged
        ]



-- }}}
