module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons as Icons
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Ports



-- {{{ MODEL


type alias Model =
    { midiStatus : MIDIStatus
    , mode : Mode
    , page : Page
    , popup : Maybe PopUp
    }


type MIDIStatus
    = Initialising
    | FailedToEstablishMIDI
    | MidiAvailable (List ( String, String ))
    | MidiConnected String


type Mode
    = Normal
    | Edit


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
    | Space


type EditOperation
    = Add
    | Remove


type alias ButtonState =
    { status : ButtonStatus
    , label : String
    , noteNumber : Int
    }


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

        Space ->
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

        Space ->
            if currentId == id then
                updateFn toUpdate

            else
                Space


addSpace : Controller -> Controller
addSpace controller =
    case controller of
        Row subControls ->
            subControls
                ++ [ Space ]
                |> Row

        Column subControls ->
            subControls
                ++ [ Space ]
                |> Column

        _ ->
            controller


removeItem : Controller -> Controller
removeItem controller =
    case controller of
        Row subControls ->
            subControls
                |> List.reverse
                |> List.drop 1
                |> List.reverse
                |> Row

        Column subControls ->
            subControls
                |> List.reverse
                |> List.drop 1
                |> List.reverse
                |> Column

        _ ->
            controller


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
      , mode = Normal
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
        { gapSize = 2
        , debug = True
        }
    }


isomorphicKeyboard : Controller
isomorphicKeyboard =
    let
        noteRange =
            List.range 36 127

        rowNumbers =
            List.range 0 10
    in
    List.map (makeIsomorphicRow noteRange 5 18) rowNumbers
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
    | ToggleNormalEdit
    | OpenMidiMenu
    | ConnectToDevice String
    | ConnectedToDevice String
    | AddSpace String
    | RemoveItem String
    | ButtonDown String
    | ButtonUp String
    | ClosePopUp


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

        ToggleNormalEdit ->
            ( { model
                | mode =
                    case model.mode of
                        Edit ->
                            Normal

                        Normal ->
                            Edit
              }
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

        AddSpace id ->
            let
                page =
                    model.page

                updatedPage =
                    { page
                        | controller =
                            updateWithId
                                "0"
                                page.controller
                                { id = id, updateFn = addSpace }
                    }
            in
            ( { model | page = updatedPage }
            , Cmd.none
            )

        RemoveItem id ->
            let
                page =
                    model.page

                updatedPage =
                    { page
                        | controller =
                            updateWithId
                                "0"
                                page.controller
                                { id = id, updateFn = removeItem }
                    }
            in
            ( { model | page = updatedPage }
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

        ClosePopUp ->
            ( { model | popup = Nothing }
            , Cmd.none
            )



-- }}}
-- {{{ VIEW


view : Model -> Html Msg
view model =
    layout
        ((case model.popup of
            Just (MidiMenu state) ->
                (inFront <|
                    el
                        (Background.color (rgba 0.5 0.5 0.5 0.8)
                            :: fillSpace
                        )
                        (midiMenu state.devices)
                )
                    :: fillSpace

            Nothing ->
                fillSpace
         )
            ++ [ Font.family
                    [ Font.external
                        { name = "Space Mono"
                        , url = "https://fonts.googleapis.com/css?family=Space+Mono"
                        }
                    , Font.monospace
                    ]
               ]
        )
    <|
        row fillSpace
            [ column
                [ height fill
                , padding 5
                , spacing 5
                , Border.widthEach { bottom = 0, top = 0, left = 0, right = 4 }
                ]
                [ Input.button
                    [ padding 10
                    , Border.rounded 10
                    , Border.width 4
                    ]
                    { onPress = Just OpenMidiMenu
                    , label =
                        Icons.gitPullRequest
                            |> Icons.withSize 36
                            |> Icons.toHtml []
                            |> html
                    }
                , Input.button
                    [ padding 10
                    , Border.rounded 10
                    , Border.width 4
                    , Background.color <|
                        case model.mode of
                            Normal ->
                                rgb 1.0 1.0 1.0

                            Edit ->
                                rgb 0.4 0.4 0.4
                    ]
                    { onPress = Just ToggleNormalEdit
                    , label =
                        Icons.edit
                            |> Icons.withSize 36
                            |> Icons.toHtml []
                            |> html
                    }
                ]
            , column
                (padding 5 :: fillSpace)
                [ midiStatus model.midiStatus
                , renderPage model.mode model.page
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


renderPage : Mode -> Page -> Element Msg
renderPage mode page =
    let
        { config, controller } =
            page
    in
    el
        ([ Border.rounded 10
         , Border.solid
         , Border.width 2
         ]
            ++ fillSpace
            ++ (case mode of
                    Normal ->
                        []

                    Edit ->
                        [ paddingXY config.gapSize 0
                        ]
               )
        )
    <|
        renderController mode config [] controller 0


renderController : Mode -> PageConfig -> List String -> Controller -> Int -> Element Msg
renderController mode config idParts controller id =
    let
        updatedParts =
            String.fromInt id :: idParts
    in
    case controller of
        Module _ subControls ->
            el
                ([ padding config.gapSize
                 , spacing config.gapSize
                 , Border.dotted
                 , Border.width 2
                 ]
                    ++ fillSpace
                )
                (renderController mode config updatedParts subControls 0)

        Row subControls ->
            case mode of
                Normal ->
                    row
                        ([ paddingXY config.gapSize 0
                         , spacingXY config.gapSize 0
                         ]
                            ++ fillSpace
                        )
                    <|
                        List.map2
                            (renderController mode config updatedParts)
                            subControls
                            (List.range 0 <| List.length subControls)

                Edit ->
                    row ([ paddingXY 5 0, spacing 5 ] ++ fillSpace)
                        [ renderEditButton config
                            Remove
                            (updatedParts
                                |> List.reverse
                                |> String.join "_"
                            )
                        , row
                            ([ spacingXY config.gapSize 0
                             , padding config.gapSize
                             , Border.width 2
                             , Border.rounded 10
                             ]
                                ++ fillSpace
                            )
                          <|
                            List.map2
                                (renderController mode config updatedParts)
                                subControls
                                (List.range 0 <| List.length subControls)
                        , renderEditButton
                            config
                            Add
                            (updatedParts
                                |> List.reverse
                                |> String.join "_"
                            )
                        ]

        Column subControls ->
            case mode of
                Normal ->
                    column
                        ([ paddingXY 0 config.gapSize
                         , spacingXY 0 config.gapSize
                         ]
                            ++ fillSpace
                        )
                    <|
                        List.map2
                            (renderController mode config updatedParts)
                            subControls
                            (List.range 0 <| List.length subControls)

                Edit ->
                    column ([ paddingXY 5 0, spacing 5 ] ++ fillSpace)
                        [ renderEditButton config
                            Remove
                            (updatedParts
                                |> List.reverse
                                |> String.join "_"
                            )
                        , column
                            ([ spacingXY 0 config.gapSize
                             , padding config.gapSize
                             , Border.width 2
                             , Border.rounded 10
                             ]
                                ++ fillSpace
                            )
                          <|
                            List.map2
                                (renderController mode config updatedParts)
                                subControls
                                (List.range 0 <| List.length subControls)
                        , renderEditButton
                            config
                            Add
                            (updatedParts
                                |> List.reverse
                                |> String.join "_"
                            )
                        ]

        Button state ->
            renderButton
                config
                state
                (updatedParts
                    |> List.reverse
                    |> String.join "_"
                )

        Space ->
            el ([ Background.color <| rgb 0.9 0.9 0.9, Border.rounded 10 ] ++ fillSpace) none


renderButton : PageConfig -> ButtonState -> String -> Element Msg
renderButton config state id =
    el
        ([ padding config.gapSize
         , spacing config.gapSize
         , Border.width 2
         , Border.rounded 10
         , Border.solid
         , case state.status of
            Off ->
                if modBy 12 state.noteNumber == 0 then
                    Background.color <| rgb255 100 100 200

                else if List.member (modBy 12 state.noteNumber) [ 1, 3, 6, 8, 10 ] then
                    Background.color <| rgb255 170 170 18

                else
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


renderEditButton : PageConfig -> EditOperation -> String -> Element Msg
renderEditButton config editOperation parentId =
    case editOperation of
        Add ->
            Input.button
                [ padding config.gapSize
                , spacing config.gapSize
                , Border.rounded 10
                , Border.width 2
                ]
                { onPress = Just <| AddSpace parentId
                , label =
                    Icons.plus
                        |> Icons.withSize 36
                        |> Icons.toHtml []
                        |> html
                }

        Remove ->
            Input.button
                [ padding config.gapSize
                , spacing config.gapSize
                , Border.rounded 10
                , Border.width 2
                ]
                { onPress = Just <| RemoveItem parentId
                , label =
                    Icons.minus
                        |> Icons.withSize 36
                        |> Icons.toHtml []
                        |> html
                }


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
