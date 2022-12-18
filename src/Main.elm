module Main exposing (..)

import Array exposing (Array)
import Browser
import Controller exposing (Controller)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import FeatherIcons as Icons
import Html exposing (Html)
import Html.Attributes exposing (name)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Midi exposing (Status(..))
import Ports



-- {{{ MODEL


type alias Model =
    { midiStatus : Status
    , mode : Mode
    , page : Page
    , popup : Maybe PopUp
    }


type Mode
    = Normal
    | Edit


type PopUp
    = MidiMenu
    | EditMenu String EditMenuState


type EditMenuState
    = EditModule
    | EditColumn
    | EditRow
    | EditButton EditButtonState


type alias Page =
    { label : String
    , controller : Controller
    , config : PageConfig
    }


type alias PageConfig =
    { gapSize : Int
    , debug : Bool
    }


type alias EditButtonState =
    { label : String
    , channel : String
    , noteNumber : String
    , velocity : String
    }


defaultButton : EditButtonState
defaultButton =
    { label = ""
    , channel = "1"
    , noteNumber = "60"
    , velocity = "100"
    }


editStateToButton : EditButtonState -> Maybe Controller
editStateToButton { label, noteNumber, channel, velocity } =
    if String.isEmpty label then
        Nothing

    else
        case
            ( Controller.stringToChannel channel
            , String.toInt noteNumber
            , String.toInt velocity
            )
        of
            ( Just ch, Just nn, Just vel ) ->
                Controller.newButton label ch nn vel
                    |> Just

            _ ->
                Nothing


init : ( Model, Cmd Msg )
init =
    ( { midiStatus = Midi.Initialising
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
        |> Controller.Column


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
        |> List.map (\i -> Controller.newButton (String.fromInt i) Controller.Ch7 i 100)
        |> Controller.Row



-- }}}
-- {{{ UPDATE


type Msg
    = MidiDevicesChanged (List Midi.Device)
    | ToggleNormalEdit
    | OpenMidiMenu
    | AddSpace String
    | RemoveItem String
    | OpenEditController String
    | SetEditType EditMenuState
    | UpdateControllerState EditMenuState
    | FinishedEdit Controller
    | ButtonDown String
    | ButtonUp String
    | ClosePopUp
    | IncomingMidi (Array Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MidiDevicesChanged devices ->
            ( { model | midiStatus = Midi.MidiAvailable devices }
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
                            Controller.updateWithId
                                "0"
                                page.controller
                                { id = id, updateFn = Controller.addSpace }
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
                            Controller.updateWithId
                                "0"
                                page.controller
                                { id = id, updateFn = Controller.removeItem }
                    }
            in
            ( { model | page = updatedPage }
            , Cmd.none
            )

        OpenEditController id ->
            let
                page =
                    model.page

                controller =
                    Controller.getWithId "0" id page.controller
            in
            ( { model
                | popup =
                    case controller of
                        Just (Controller.Button { noteNumber, label, channel, velocity }) ->
                            Just <|
                                EditMenu id <|
                                    EditButton
                                        { noteNumber = String.fromInt noteNumber
                                        , label = label
                                        , channel = Controller.channelToString channel
                                        , velocity = String.fromInt velocity
                                        }

                        _ ->
                            Nothing
              }
            , Cmd.none
            )

        SetEditType editType ->
            let
                newPopup =
                    case model.popup of
                        Just (EditMenu id _) ->
                            Just <| EditMenu id editType

                        _ ->
                            Nothing
            in
            ( { model | popup = newPopup }, Cmd.none )

        FinishedEdit controller ->
            case model.popup of
                Just (EditMenu id _) ->
                    let
                        page =
                            model.page

                        updatedPage =
                            { page
                                | controller =
                                    Controller.updateWithId
                                        "0"
                                        page.controller
                                        { id = id, updateFn = \_ -> controller }
                            }
                    in
                    ( { model
                        | popup = Nothing
                        , page = updatedPage
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | popup = Nothing }
                    , Cmd.none
                    )

        UpdateControllerState state ->
            case model.popup of
                Just (EditMenu id _) ->
                    ( { model
                        | popup =
                            Just <|
                                EditMenu id state
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | popup = Nothing }
                    , Cmd.none
                    )

        ButtonDown id ->
            let
                page =
                    model.page

                button =
                    Controller.getWithId "0" id page.controller

                updatedPage =
                    { page
                        | controller =
                            Controller.updateWithId
                                "0"
                                page.controller
                                { id = id, updateFn = Controller.buttonOn }
                    }
            in
            ( { model | page = updatedPage }
            , case button of
                Just (Controller.Button state) ->
                    Ports.sendNoteOn
                        { noteNumber = state.noteNumber
                        , velocity = state.velocity
                        , channel = Controller.channelToMidiNumber state.channel
                        }

                _ ->
                    Cmd.none
            )

        ButtonUp id ->
            let
                page =
                    model.page

                button =
                    Controller.getWithId "0" id page.controller

                updatedPage =
                    { page
                        | controller =
                            Controller.updateWithId
                                "0"
                                page.controller
                                { id = id, updateFn = Controller.buttonOff }
                    }
            in
            ( { model | page = updatedPage }
            , case button of
                Just (Controller.Button state) ->
                    Ports.sendNoteOff
                        { noteNumber = state.noteNumber
                        , velocity = 0
                        , channel = Controller.channelToMidiNumber state.channel
                        }

                _ ->
                    Cmd.none
            )

        ClosePopUp ->
            ( { model | popup = Nothing }
            , Cmd.none
            )

        IncomingMidi midiMsg ->
            let
                _ =
                    Debug.log "Msg" <| Midi.intArrayToMidiMsg midiMsg
            in
            ( model
            , Cmd.none
            )



-- }}}
-- {{{ VIEW


view : Model -> Html Msg
view model =
    layout
        ((case model.popup of
            Just popup ->
                (inFront <|
                    case popup of
                        MidiMenu ->
                            el
                                (Background.color (rgba 0.5 0.5 0.5 0.8)
                                    :: fillSpace
                                )
                                (case model.midiStatus of
                                    Midi.MidiAvailable devices ->
                                        midiMenu devices

                                    _ ->
                                        midiMenu []
                                )

                        EditMenu _ state ->
                            el
                                (Background.color (rgba 0.5 0.5 0.5 0.8)
                                    :: fillSpace
                                )
                                (editMenu state)
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
        row
            fillSpace
            [ column
                [ height fill
                , padding 5
                , spacing 5
                , Border.widthEach { bottom = 0, top = 0, left = 0, right = 4 }
                ]
                [ Input.button
                    [ padding 10
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
            , el
                (padding 5 :: fillSpace)
              <|
                renderPage model.mode model.page
            ]


midiMenu : List Midi.Device -> Element Msg
midiMenu devices =
    el [ centerX, centerY ] <|
        column
            [ padding 10
            , spacing 10
            , Background.color (rgb 1.0 1.0 1.0)
            ]
            (paragraph [] [ text "MIDI Devices" ]
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


deviceItem : Midi.Device -> Element Msg
deviceItem { name, input, output } =
    row
        [ padding 10
        , spacing 10
        , width fill
        ]
        [ text name
        , text <|
            if Maybe.withDefault False input then
                "True"

            else
                "False"
        , text <|
            if Maybe.withDefault False output then
                "True"

            else
                "False"
        ]


editMenu : EditMenuState -> Element Msg
editMenu menuType =
    wrappedRow [ centerX, centerY, spacing 10 ]
        [ el
            [ alignTop
            , padding 10
            , spacing 10
            , Background.color (rgb 1.0 1.0 1.0)
            ]
          <|
            Input.radio
                [ spacing 10 ]
                { onChange = SetEditType
                , selected = Just menuType
                , label = Input.labelAbove [] (text "Type")
                , options =
                    [ Input.option EditModule (text "Module")
                    , Input.option EditColumn (text "Column")
                    , Input.option EditRow (text "Row")
                    , Input.option
                        (case menuType of
                            EditButton _ ->
                                menuType

                            _ ->
                                EditButton defaultButton
                        )
                        (text "Button")
                    ]
                }
        , case menuType of
            EditButton state ->
                column
                    [ padding 10
                    , spacing 10
                    , Background.color (rgb 1.0 1.0 1.0)
                    ]
                    [ Input.text
                        [ Border.width 2
                        , Border.rounded 0
                        , Border.color (rgb 0.0 0.0 0.0)
                        ]
                        { onChange =
                            \newLabel ->
                                { state | label = newLabel }
                                    |> EditButton
                                    |> UpdateControllerState
                        , text = state.label
                        , placeholder = Just <| Input.placeholder [] (text "enter label")
                        , label = Input.labelAbove [] (text "Label")
                        }
                    , Input.text
                        [ Border.width 2
                        , Border.rounded 0
                        , Border.color (rgb 0.0 0.0 0.0)
                        ]
                        { onChange =
                            \newChannel ->
                                { state | channel = newChannel }
                                    |> EditButton
                                    |> UpdateControllerState
                        , text = state.channel
                        , placeholder = Just <| Input.placeholder [] (text "enter channel#")
                        , label = Input.labelAbove [] (text "Channel")
                        }
                    , Input.text
                        [ Border.width 2
                        , Border.rounded 0
                        , Border.color (rgb 0.0 0.0 0.0)
                        ]
                        { onChange =
                            \newNoteNumber ->
                                { state | noteNumber = newNoteNumber }
                                    |> EditButton
                                    |> UpdateControllerState
                        , text = state.noteNumber
                        , placeholder = Just <| Input.placeholder [] (text "enter note#")
                        , label = Input.labelAbove [] (text "Note Number")
                        }
                    , Input.text
                        [ Border.width 2
                        , Border.rounded 0
                        , Border.color (rgb 0.0 0.0 0.0)
                        ]
                        { onChange =
                            \newVelocity ->
                                { state | velocity = newVelocity }
                                    |> EditButton
                                    |> UpdateControllerState
                        , text = state.velocity
                        , placeholder = Just <| Input.placeholder [] (text "enter velocity")
                        , label = Input.labelAbove [] (text "Velocity")
                        }
                    , row [ spacing 2 ]
                        [ case editStateToButton state of
                            Just controller ->
                                Input.button
                                    [ padding 5
                                    , Border.width 2
                                    , Border.solid
                                    , Border.color <| rgb 0 0 0
                                    ]
                                    { onPress = Just <| FinishedEdit controller
                                    , label = text "Ok"
                                    }

                            Nothing ->
                                Input.button
                                    [ padding 5
                                    , Border.width 2
                                    , Border.solid
                                    , Border.color <| rgb 0.7 0.7 0.7
                                    , Font.color <| rgb 0.7 0.7 0.7
                                    ]
                                    { onPress = Nothing
                                    , label = text "Ok"
                                    }
                        , Input.button
                            [ padding 5
                            , Border.width 2
                            , Border.solid
                            , Border.color <| rgb255 0 0 0
                            ]
                            { onPress = Just ClosePopUp, label = text "Cancel" }
                        ]
                    ]

            _ ->
                column
                    [ padding 10
                    , spacing 10
                    , Background.color (rgb 1.0 1.0 1.0)
                    ]
                    [ Input.button
                        [ padding 5
                        , Border.width 2
                        , Border.solid
                        , Border.color <| rgb255 0 0 0
                        ]
                        { onPress = Just ClosePopUp, label = text "Cancel" }
                    ]
        ]


renderPage : Mode -> Page -> Element Msg
renderPage mode page =
    let
        { config, controller } =
            page
    in
    el
        (fillSpace
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
        Controller.Module _ subControls ->
            Lazy.lazy2
                el
                ([ padding config.gapSize
                 , spacing config.gapSize
                 , Border.dotted
                 , Border.width 2
                 ]
                    ++ fillSpace
                )
                (renderController mode config updatedParts subControls 0)

        Controller.Row subControls ->
            case mode of
                Normal ->
                    Lazy.lazy2 row
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
                    Lazy.lazy2 row
                        ([ paddingXY 5 0
                         , spacing 5
                         , Border.width 2
                         , Border.dashed
                         ]
                            ++ fillSpace
                        )
                        [ renderEditButton config
                            Controller.Remove
                            (updatedParts
                                |> List.reverse
                                |> String.join "_"
                            )
                        , row
                            ([ spacingXY config.gapSize 0
                             , padding config.gapSize
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
                            Controller.Add
                            (updatedParts
                                |> List.reverse
                                |> String.join "_"
                            )
                        ]

        Controller.Column subControls ->
            case mode of
                Normal ->
                    Lazy.lazy2 column
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
                    Lazy.lazy2 column
                        ([ paddingXY 0 5
                         , spacing 5
                         , Border.width 2
                         , Border.dashed
                         ]
                            ++ fillSpace
                        )
                        [ renderEditButton config
                            Controller.Remove
                            (updatedParts
                                |> List.reverse
                                |> String.join "_"
                            )
                        , column
                            ([ spacingXY 0 config.gapSize
                             , padding config.gapSize
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
                            Controller.Add
                            (updatedParts
                                |> List.reverse
                                |> String.join "_"
                            )
                        ]

        Controller.Button state ->
            renderButton
                config
                mode
                state
                (updatedParts
                    |> List.reverse
                    |> String.join "_"
                )

        Controller.Space ->
            el ((Background.color <| rgb 0.9 0.9 0.9) :: fillSpace) none


renderButton : PageConfig -> Mode -> Controller.ButtonState -> String -> Element Msg
renderButton config mode state id =
    case mode of
        Normal ->
            el
                ([ padding 0
                 , spacing 0
                 , Border.width 2

                 -- , Border.solid
                 , case state.status of
                    Controller.Off ->
                        if modBy 12 state.noteNumber == 0 then
                            Background.color <| rgb255 100 100 200

                        else if List.member (modBy 12 state.noteNumber) [ 1, 3, 6, 8, 10 ] then
                            Background.color <| rgb255 170 170 18

                        else
                            Background.color <| rgb255 221 221 23

                    Controller.On ->
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
                        Controller.Off ->
                            "Off\n" ++ Controller.channelToString state.channel ++ "\n"

                        Controller.On ->
                            "Off\n" ++ Controller.channelToString state.channel ++ "\n"

                  else
                    ""
                 )
                    ++ state.label
                    |> text
                    |> el
                        [ centerX
                        , centerY
                        , Font.size 14
                        ]
                )

        Edit ->
            Input.button
                ([ padding config.gapSize
                 , spacing config.gapSize
                 , Border.width 2
                 , Border.dashed
                 , Font.size 14
                 , if modBy 12 state.noteNumber == 0 then
                    Background.color <| rgb255 100 100 200

                   else if List.member (modBy 12 state.noteNumber) [ 1, 3, 6, 8, 10 ] then
                    Background.color <| rgb255 170 170 18

                   else
                    Background.color <| rgb255 221 221 23
                 ]
                    ++ fillSpace
                )
                { onPress = Just <| OpenEditController id
                , label =
                    state.label
                        |> text
                }


renderEditButton : PageConfig -> Controller.EditOperation -> String -> Element Msg
renderEditButton config editOperation parentId =
    case editOperation of
        Controller.Add ->
            Input.button
                [ centerX
                , padding config.gapSize
                , spacing config.gapSize
                , Border.width 2
                ]
                { onPress = Just <| AddSpace parentId
                , label =
                    Icons.plus
                        |> Icons.withSize 36
                        |> Icons.toHtml []
                        |> html
                }

        Controller.Remove ->
            Input.button
                [ centerX
                , padding config.gapSize
                , spacing config.gapSize
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
        [ Ports.midiDevices MidiDevicesChanged
        , Ports.incomingMidi IncomingMidi
        ]



-- }}}
