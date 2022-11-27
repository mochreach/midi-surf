module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import FeatherIcons as Icons
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Material.Icons exposing (model_training)
import Ports exposing (listenForMIDIStatus)



-- {{{ MODEL


type alias Model =
    { midiStatus : MIDIStatus
    , page : Page
    , popup : Maybe PopUp
    }


type MIDIStatus
    = Initialising
    | FailedToEstablishMIDI
    | MidiAvailable (List ( String, String ))
    | MidiConnected String


type PopUp
    = MidiMenu MidiMenuModel


type alias MidiMenuModel =
    { devices : List ( String, String )
    , selected : Maybe String
    }


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
      , popup = Nothing
      }
    , Cmd.none
    )


defaultPage : Page
defaultPage =
    { label = "1"
    , controller = isomorphicKeyboard
    , config =
        { gapSize = 5
        , debug = True
        }
    }


isomorphicKeyboard : Controller
isomorphicKeyboard =
    let
        noteRange =
            List.range 36 128

        rowNumbers =
            List.range 2 8
    in
    List.map (makeIsomorphicRow noteRange 5 12) rowNumbers
        |> List.reverse
        |> Column


makeIsomorphicRow : List Int -> Int -> Int -> Int -> Controller
makeIsomorphicRow noteRange offset rowLength rowNumber =
    let
        start =
            offset * rowNumber

        includedRange =
            List.range start (start + rowLength)
    in
    noteRange
        |> List.indexedMap Tuple.pair
        |> List.filter (\( i, _ ) -> List.member i includedRange)
        |> List.map Tuple.second
        |> List.map (\i -> newButton (String.fromInt i) i)
        |> Row



-- }}}
-- {{{ UPDATE


type Msg
    = MIDIStatusChanged (List ( String, String ))
    | OpenMidiMenu
    | ConnectToDevice String
    | ConnectedToDevice String
    | ClosePopUp
    | ButtonDown String
    | ButtonUp String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MIDIStatusChanged devices ->
            ( if List.isEmpty devices |> not then
                { model | midiStatus = MidiAvailable devices }

              else
                { model | midiStatus = FailedToEstablishMIDI }
            , Cmd.none
            )

        OpenMidiMenu ->
            ( { model
                | popup =
                    Just <|
                        MidiMenu
                            { devices =
                                case model.midiStatus of
                                    MidiAvailable devices ->
                                        devices

                                    _ ->
                                        []
                            , selected = Nothing
                            }
              }
            , Cmd.none
            )

        ConnectToDevice id ->
            ( model
            , Ports.connectToDevice id
            )

        ConnectedToDevice name ->
            ( { model
                | midiStatus = MidiConnected name
                , popup = Nothing
              }
            , Cmd.none
            )

        ClosePopUp ->
            ( { model | popup = Nothing }
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


view : Model -> Html Msg
view model =
    layout
        (case model.popup of
            Just (MidiMenu state) ->
                (inFront <|
                    el
                        ([ Background.color (rgba 0.5 0.5 0.5 0.8)
                         ]
                            ++ fillSpace
                        )
                        (midiMenu state.devices)
                )
                    :: fillSpace

            Nothing ->
                fillSpace
        )
    <|
        row fillSpace
            [ column
                [ height fill, padding 5 ]
                [ Input.button
                    [ padding 10
                    , Border.rounded 10
                    , Border.width 5
                    ]
                    { onPress = Just OpenMidiMenu
                    , label =
                        Icons.gitPullRequest
                            |> Icons.withSize 36
                            |> Icons.toHtml []
                            |> html
                    }
                ]
            , column
                (padding 5 :: fillSpace)
                [ midiStatus model.midiStatus
                , renderPage model.page
                ]
            ]


midiStatus : MIDIStatus -> Element Msg
midiStatus status =
    case status of
        Initialising ->
            paragraph [] [ text "Initialising..." ]

        FailedToEstablishMIDI ->
            paragraph [] [ text "Failed to establish MIDI connection." ]

        MidiAvailable devices ->
            paragraph [] [ text <| "MIDI connection: " ++ String.fromInt (List.length devices) ]

        MidiConnected device ->
            paragraph [] [ text <| "MIDI Out: " ++ device ]


midiMenu : List ( String, String ) -> Element Msg
midiMenu devices =
    el [ centerX, centerY ] <|
        column
            [ padding 10
            , spacing 10
            , Background.color (rgb 1.0 1.0 1.0)
            ]
            (paragraph [] [ text "Select MIDI Device" ]
                :: (case devices of
                        [] ->
                            [ paragraph [] [ text "No MIDI devices connected." ] ]

                        _ ->
                            List.map deviceItem devices
                   )
                ++ [ Input.button
                        [ padding 5
                        , Border.width 2
                        , Border.solid
                        , Border.color <| rgb255 0 0 0
                        ]
                        { onPress = Just ClosePopUp, label = text "Cancel" }
                   ]
            )


deviceItem : ( String, String ) -> Element Msg
deviceItem ( id, name ) =
    Input.button
        [ padding 10
        , width fill
        , Background.color <| rgb 0.8 0.8 0.8
        ]
        { onPress = Just <| ConnectToDevice id
        , label = text name
        }


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
                if modBy 12 state.noteNumber /= 0 then
                    Background.color <| rgb255 221 221 23

                else
                    Background.color <| rgb255 100 100 200

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
        ((if config.debug then
            case state.status of
                Off ->
                    "Off\n"

                On ->
                    "On\n"

          else
            ""
         )
            ++ state.label
            |> text
        )


fillSpace : List (Attribute msg)
fillSpace =
    [ height fill, width fill ]



-- }}}
-- {{{ PROGRAM


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.listenForMIDIStatus MIDIStatusChanged
        , Ports.connectedToDevice ConnectedToDevice
        ]



-- }}}
