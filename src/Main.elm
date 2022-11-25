module Main exposing (..)

import Animator
import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Ports exposing (listenForMIDIStatus)
import Time



-- {{{ MODEL


type alias Model =
    { midiStatus : MIDIStatus
    , controlState : Animator.Timeline (Dict String ControlState)
    , page : Page
    }


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.watching
            .controlState
            (\newControlState model ->
                { model | controlState = newControlState }
            )


type MIDIStatus
    = Initialising
    | FailedToEstablishMIDI
    | MIDIConnected


type alias Page =
    { label : String
    , gapSize : Int
    , controller : Controller
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
                |> Debug.log "IDs"
                |> Animator.init
      , page = defaultPage
      }
    , Cmd.none
    )


defaultPage : Page
defaultPage =
    { label = "1"
    , gapSize = 5
    , controller =
        Control Button
            |> List.repeat 12
            |> Row
            |> List.repeat 3
            |> Column
    }



-- }}}
-- {{{ UPDATE


type Msg
    = AnimationStep Time.Posix
    | MIDIStatusChanged Bool
    | ButtonDown String
    | ButtonUp String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        maybeAlways value =
            Maybe.map (\_ -> value)

        setControlState id newState =
            Dict.update id (maybeAlways newState) <| Animator.current model.controlState
    in
    case msg of
        AnimationStep newTime ->
            ( Animator.update newTime animator model
            , Cmd.none
            )

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
                    Animator.go Animator.slowly (setControlState id Pressed) model.controlState
              }
            , Cmd.none
            )

        ButtonUp id ->
            ( { model
                | controlState =
                    Animator.go Animator.slowly (setControlState id Default) model.controlState
              }
            , Cmd.none
            )



-- }}}
-- {{{ VIEW


view : Model -> Element Msg
view model =
    column (padding 5 :: fillSpace)
        [ midiStatus model.midiStatus
        , renderPage (Animator.current model.controlState) model.page
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
        { gapSize, controller } =
            page
    in
    el ([ padding gapSize, Border.solid, Border.width 2 ] ++ fillSpace) <|
        renderController controlStates gapSize [] controller 0


renderController : Dict String ControlState -> Int -> List String -> Controller -> Int -> Element Msg
renderController controlStates gapSize idParts controller id =
    let
        updatedParts =
            String.fromInt id :: idParts
    in
    case controller of
        Module _ _ ->
            el
                ([ padding gapSize
                 , spacing gapSize
                 , Background.color <| rgb255 3 5 5
                 ]
                    ++ fillSpace
                )
                none
                |> Debug.todo "Needs to do stuff"

        Row subControls ->
            row
                ([ padding gapSize
                 , spacing gapSize
                 , Background.color <| rgb255 21 39 200
                 ]
                    ++ fillSpace
                )
            <|
                List.map2
                    (renderController controlStates gapSize updatedParts)
                    subControls
                    (List.range 0 <| List.length subControls)

        Column subControls ->
            column
                ([ padding gapSize
                 , spacing gapSize
                 , Background.color <| rgb255 21 221 23
                 ]
                    ++ fillSpace
                )
            <|
                List.map2
                    (renderController controlStates gapSize updatedParts)
                    subControls
                    (List.range 0 <| List.length subControls)

        Control midiControl ->
            renderMidiControl
                controlStates
                gapSize
                (updatedParts
                    |> List.reverse
                    |> String.join "_"
                )
                midiControl


renderMidiControl : Dict String ControlState -> Int -> String -> MidiControl -> Element Msg
renderMidiControl controlStates gapSize id midiControl =
    let
        controlState =
            Dict.get id controlStates
                |> Maybe.withDefault Default
    in
    case midiControl of
        Button ->
            el
                ([ padding gapSize
                 , spacing gapSize
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
                none


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
subscriptions model =
    Sub.batch
        [ listenForMIDIStatus MIDIStatusChanged
        , Animator.toSubscription AnimationStep model animator
        ]



-- }}}
