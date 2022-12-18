module Main exposing (..)

import Array exposing (Array)
import Browser
import Controller exposing (Controller)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import FeatherIcons as Icons
import Html exposing (Html)
import Html.Attributes exposing (name)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Midi exposing (MidiMsg(..), Status(..))
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
    | Edit Bool


type PopUp
    = MidiMenu
    | EditMenu String EditMenuState


type EditMenuState
    = EditModule
    | EditColumn (List Controller)
    | EditRow (List Controller)
    | EditButton EditButtonState
    | EditSpace


updateWithMidiMsg : MidiMsg -> EditMenuState -> EditMenuState
updateWithMidiMsg midiMsg state =
    case state of
        EditModule ->
            state

        EditColumn subControls ->
            case midiMsg of
                Midi.NoteOn { channel, pitch, velocity } ->
                    let
                        ch =
                            Controller.midiNumberToChannel channel
                                |> Maybe.withDefault Controller.Ch1

                        label =
                            "Ch"
                                ++ Controller.channelToString ch
                                ++ "#"
                                ++ String.fromInt pitch
                    in
                    List.append subControls [ Controller.newButton label ch pitch velocity ]
                        |> EditColumn

                _ ->
                    state

        EditRow subControls ->
            case midiMsg of
                Midi.NoteOn { channel, pitch, velocity } ->
                    let
                        ch =
                            Controller.midiNumberToChannel channel
                                |> Maybe.withDefault Controller.Ch1

                        label =
                            "Ch"
                                ++ Controller.channelToString ch
                                ++ "#"
                                ++ String.fromInt pitch
                    in
                    List.append subControls [ Controller.newButton label ch pitch velocity ]
                        |> EditRow

                _ ->
                    state

        EditButton buttonState ->
            updateEditButtonWithMidiMsg midiMsg buttonState
                |> EditButton

        EditSpace ->
            state


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


updateEditButtonWithMidiMsg : MidiMsg -> EditButtonState -> EditButtonState
updateEditButtonWithMidiMsg midiMsg state =
    case Debug.log "Msg" midiMsg of
        NoteOn noteOnParams ->
            { state
              -- Adding 1 to the channel so that they're labelled 1-16
                | channel = String.fromInt (noteOnParams.channel + 1)
                , noteNumber = String.fromInt noteOnParams.pitch
                , velocity = String.fromInt noteOnParams.velocity
            }

        _ ->
            state


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
                -- TODO: These values should not exceed 127, handle with midi module
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
                        Normal ->
                            Edit False

                        Edit False ->
                            Edit True

                        Edit True ->
                            Normal
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
                            EditButton
                                { noteNumber = String.fromInt noteNumber
                                , label = label
                                , channel = Controller.channelToString channel
                                , velocity = String.fromInt velocity
                                }
                                |> EditMenu id
                                |> Just

                        Just (Controller.Row subControls) ->
                            EditRow subControls
                                |> EditMenu id
                                |> Just

                        Just (Controller.Column subControls) ->
                            EditColumn subControls
                                |> EditMenu id
                                |> Just

                        Just Controller.Space ->
                            EditSpace
                                |> EditMenu id
                                |> Just

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
                        , mode =
                            case model.mode of
                                Normal ->
                                    Normal

                                Edit False ->
                                    Normal

                                Edit True ->
                                    Edit True
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model
                        | popup = Nothing
                        , mode = Normal
                      }
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
            ( { model
                | popup =
                    case model.popup of
                        Just (EditMenu id state) ->
                            updateWithMidiMsg (Midi.intArrayToMidiMsg midiMsg) state
                                |> EditMenu id
                                |> Just

                        _ ->
                            model.popup
              }
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

                            Edit False ->
                                rgb 0.4 0.4 0.4

                            Edit True ->
                                rgb 0.2 0.2 0.2
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
    wrappedRow [ centerX, padding 20, spacing 10 ]
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
                    , Input.option (EditColumn []) (text "Column")
                    , Input.option (EditRow []) (text "Row")
                    , Input.option
                        (case menuType of
                            EditButton _ ->
                                menuType

                            _ ->
                                EditButton defaultButton
                        )
                        (text "Button")
                    , Input.option EditSpace (text "Space")
                    ]
                }
        , case menuType of
            EditRow subControls ->
                editRowPane subControls

            EditColumn subControls ->
                editColumnPane subControls

            EditButton state ->
                editButtonPane state

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


editRowPane : List Controller -> Element Msg
editRowPane subControls =
    column
        [ padding 10
        , spacing 10
        , Background.color (rgb 1.0 1.0 1.0)
        ]
        (row [ spacing 10 ]
            [ Input.button
                [ padding 5
                , Border.width 2
                , Border.solid
                , Border.color <| rgb255 0 0 0
                ]
                { onPress =
                    List.append subControls [ Controller.Space ]
                        |> EditRow
                        |> UpdateControllerState
                        |> Just
                , label = text "Add"
                }
            , Input.button
                [ padding 5
                , Border.width 2
                , Border.solid
                , Border.color <| rgb255 0 0 0
                ]
                { onPress =
                    List.reverse subControls
                        |> List.tail
                        |> Maybe.withDefault []
                        |> List.reverse
                        |> EditRow
                        |> UpdateControllerState
                        |> Just
                , label = text "Remove"
                }
            ]
            :: (List.map Controller.controllerToString subControls
                    |> List.map text
               )
            ++ [ row [ spacing 2 ]
                    [ Input.button
                        [ padding 5
                        , Border.width 2
                        , Border.solid
                        , Border.color <| rgb 0 0 0
                        ]
                        { onPress = Just <| FinishedEdit <| Controller.Row subControls
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
        )


editColumnPane : List Controller -> Element Msg
editColumnPane subControls =
    column
        [ padding 10
        , spacing 10
        , Background.color (rgb 1.0 1.0 1.0)
        ]
        (row [ spacing 10 ]
            [ Input.button
                [ padding 5
                , Border.width 2
                , Border.solid
                , Border.color <| rgb255 0 0 0
                ]
                { onPress =
                    List.append subControls [ Controller.Space ]
                        |> EditColumn
                        |> UpdateControllerState
                        |> Just
                , label = text "Add"
                }
            , Input.button
                [ padding 5
                , Border.width 2
                , Border.solid
                , Border.color <| rgb255 0 0 0
                ]
                { onPress =
                    List.reverse subControls
                        |> List.tail
                        |> Maybe.withDefault []
                        |> List.reverse
                        |> EditColumn
                        |> UpdateControllerState
                        |> Just
                , label = text "Remove"
                }
            ]
            :: (List.map Controller.controllerToString subControls
                    |> List.map text
               )
            ++ [ row [ spacing 2 ]
                    [ Input.button
                        [ padding 5
                        , Border.width 2
                        , Border.solid
                        , Border.color <| rgb 0 0 0
                        ]
                        { onPress = Just <| FinishedEdit <| Controller.Column subControls
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
        )


editButtonPane : EditButtonState -> Element Msg
editButtonPane state =
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

                    Edit _ ->
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

                Edit _ ->
                    Lazy.lazy2 row
                        ([ paddingXY 5 0
                         , spacing 5
                         , Border.width 2
                         , Border.dashed
                         ]
                            ++ fillSpace
                        )
                        [ renderEditButton config
                            Controller.EditContainer
                            (updatedParts
                                |> List.reverse
                                |> String.join "_"
                            )
                        , renderEditButton config
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

                Edit _ ->
                    Lazy.lazy2 column
                        ([ paddingXY 0 5
                         , spacing 5
                         , Border.width 2
                         , Border.dashed
                         ]
                            ++ fillSpace
                        )
                        [ renderEditButton config
                            Controller.EditContainer
                            (updatedParts
                                |> List.reverse
                                |> String.join "_"
                            )
                        , renderEditButton config
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
            el
                ([ Background.color <| rgb 0.9 0.9 0.9
                 , Events.onClick <|
                    OpenEditController
                        (updatedParts
                            |> List.reverse
                            |> String.join "_"
                        )
                 ]
                    ++ fillSpace
                )
                none


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

        Edit _ ->
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
        Controller.EditContainer ->
            Input.button
                [ centerX
                , padding config.gapSize
                , spacing config.gapSize
                , Border.width 2
                ]
                { onPress = Just <| OpenEditController parentId
                , label =
                    Icons.edit2
                        |> Icons.withSize 36
                        |> Icons.toHtml []
                        |> html
                }

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
