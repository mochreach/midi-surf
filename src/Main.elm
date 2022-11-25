module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Ports exposing (listenForMIDIStatus)
import Time exposing (ZoneName(..))



-- {{{ MODEL


type alias Model =
    { midiStatus : MIDIStatus
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


type Controller
    = Module String Controller
    | Row (List Controller)
    | Column (List Controller)
    | Button ButtonState


getWithId : String -> String -> Controller -> Maybe Controller
getWithId currentId id controller =
    case controller of
        Module _ subController ->
            if currentId == id then
                Just controller

            else
                getWithId (currentId ++ "_0") id subController

        Row controllers ->
            if currentId == id then
                Just controller

            else
                List.indexedMap
                    (\i c ->
                        getWithId (currentId ++ "_" ++ String.fromInt i) id c
                    )
                    controllers
                    |> List.filterMap identity
                    |> List.head

        Column controllers ->
            if currentId == id then
                Just controller

            else
                List.indexedMap
                    (\i c ->
                        getWithId (currentId ++ "_" ++ String.fromInt i) id c
                    )
                    controllers
                    |> List.filterMap identity
                    |> List.head

        Button _ ->
            if currentId == id then
                Just controller

            else
                Nothing


updateWithId : String -> Controller -> { id : String, updateFn : Controller -> Controller } -> Controller
updateWithId currentId toUpdate updateInfo =
    let
        { id, updateFn } =
            updateInfo
    in
    case toUpdate of
        Module label controller ->
            if currentId == id then
                updateFn controller

            else
                updateWithId (currentId ++ "_0") controller updateInfo
                    |> Module label

        Row controllers ->
            if currentId == id then
                updateFn toUpdate

            else
                List.indexedMap
                    (\i c ->
                        updateWithId (currentId ++ "_" ++ String.fromInt i) c updateInfo
                    )
                    controllers
                    |> Row

        Column controllers ->
            if currentId == id then
                updateFn toUpdate

            else
                List.indexedMap
                    (\i c ->
                        updateWithId (currentId ++ "_" ++ String.fromInt i) c updateInfo
                    )
                    controllers
                    |> Column

        Button state ->
            if currentId == id then
                updateFn toUpdate

            else
                Button state


type alias ButtonState =
    { status : ButtonStatus
    , label : String
    , noteNumber : Int
    }


newButton : String -> Int -> Controller
newButton label noteNumber =
    Button
        { status = Off
        , label = label
        , noteNumber = noteNumber
        }


buttonOn : Controller -> Controller
buttonOn controller =
    case controller of
        Button state ->
            Button { state | status = On }

        _ ->
            controller


buttonOff : Controller -> Controller
buttonOff controller =
    case controller of
        Button state ->
            Button { state | status = Off }

        _ ->
            controller


type ButtonStatus
    = On
    | Off


init : ( Model, Cmd Msg )
init =
    ( { midiStatus = Initialising
      , page = defaultPage
      }
    , Cmd.none
    )


defaultPage : Page
defaultPage =
    { label = "1"
    , controller =
        List.map (\n -> newButton (String.fromInt n) n) (List.range 60 73)
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
    case msg of
        MIDIStatusChanged isConnected ->
            ( if isConnected then
                { model | midiStatus = MIDIConnected }

              else
                { model | midiStatus = FailedToEstablishMIDI }
            , Cmd.none
            )

        ButtonDown id ->
            let
                page =
                    model.page

                button =
                    getWithId "0" id page.controller

                updatedPage =
                    { page
                        | controller =
                            updateWithId
                                "0"
                                page.controller
                                { id = id, updateFn = buttonOn }
                    }
            in
            ( { model | page = updatedPage }
            , case button of
                Just (Button state) ->
                    Ports.sendNoteOn state.noteNumber

                _ ->
                    Cmd.none
            )

        ButtonUp id ->
            let
                page =
                    model.page

                button =
                    getWithId "0" id page.controller

                updatedPage =
                    { page
                        | controller =
                            updateWithId
                                "0"
                                page.controller
                                { id = id, updateFn = buttonOff }
                    }
            in
            ( { model | page = updatedPage }
            , case button of
                Just (Button state) ->
                    Ports.sendNoteOff state.noteNumber

                _ ->
                    Cmd.none
            )



-- }}}
-- {{{ VIEW


view : Model -> Element Msg
view model =
    column (padding 5 :: fillSpace)
        [ midiStatus model.midiStatus
        , renderPage model.page
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


renderPage : Page -> Element Msg
renderPage page =
    let
        { config, controller } =
            page
    in
    el ([ padding config.gapSize, Border.solid, Border.width 2 ] ++ fillSpace) <|
        renderController config [] controller 0


renderController : PageConfig -> List String -> Controller -> Int -> Element Msg
renderController config idParts controller id =
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
                (renderController config updatedParts subControls 0)

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
                    (renderController config updatedParts)
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
                    (renderController config updatedParts)
                    subControls
                    (List.range 0 <| List.length subControls)

        Button state ->
            renderButton
                config
                state
                (updatedParts
                    |> List.reverse
                    |> String.join "_"
                )


renderButton : PageConfig -> ButtonState -> String -> Element Msg
renderButton config state id =
    el
        ([ padding config.gapSize
         , spacing config.gapSize
         , case state.status of
            Off ->
                Background.color <| rgb255 221 221 23

            On ->
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
            case state.status of
                Off ->
                    text "Off"

                On ->
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
