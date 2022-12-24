module Main exposing (..)

import Array exposing (Array)
import Browser
import Controller exposing (Controller, FaderStatus(..))
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import FeatherIcons as Icons
import Html exposing (Html, p)
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
    | EditNote EditNoteState
    | EditCCValue EditCCValueState
    | EditFader EditFaderState
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
                    List.append subControls [ Controller.newNote label ch pitch velocity ]
                        |> EditColumn

                Midi.ControllerChange { channel, controller } ->
                    let
                        ch =
                            Controller.midiNumberToChannel channel
                                |> Maybe.withDefault Controller.Ch1

                        label =
                            "Ch"
                                ++ Controller.channelToString ch
                                ++ " CC "
                                ++ String.fromInt controller
                    in
                    List.append
                        subControls
                        [ Controller.Fader
                            { status = Controller.Set
                            , label = label
                            , channel = ch
                            , ccNumber = controller
                            , valuePercent = 50
                            , valueMin = 0
                            , valueMax = 127
                            }
                        ]
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
                    List.append subControls [ Controller.newNote label ch pitch velocity ]
                        |> EditRow

                Midi.ControllerChange { channel, controller } ->
                    let
                        ch =
                            Controller.midiNumberToChannel channel
                                |> Maybe.withDefault Controller.Ch1

                        label =
                            "Ch"
                                ++ Controller.channelToString ch
                                ++ " CC "
                                ++ String.fromInt controller
                    in
                    List.append
                        subControls
                        [ Controller.Fader
                            { status = Controller.Set
                            , label = label
                            , channel = ch
                            , ccNumber = controller
                            , valuePercent = 50
                            , valueMin = 0
                            , valueMax = 127
                            }
                        ]
                        |> EditRow

                _ ->
                    state

        EditNote noteState ->
            updateEditNoteWithMidiMsg midiMsg noteState
                |> EditNote

        EditCCValue ccState ->
            updateEditCCValueWithMidiMsg midiMsg ccState
                |> EditCCValue

        EditFader faderState ->
            updateEditFaderWithMidiMsg midiMsg faderState
                |> EditFader

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


getControllerFromActivePage : String -> Int -> Array Page -> Maybe Controller
getControllerFromActivePage id activePage pages =
    pages
        |> Array.get activePage
        |> Maybe.map .controller
        |> Maybe.andThen (Controller.getWithId "0" id)


updateControllerOnActivePage :
    Int
    -> Array Page
    -> { id : String, updateFn : Controller -> Controller }
    -> Array Page
updateControllerOnActivePage activePage pages updateInfo =
    pages
        |> Array.indexedMap Tuple.pair
        |> Array.map
            (\( i, p ) ->
                if i == activePage then
                    { p
                        | controller =
                            Controller.updateWithId "0" p.controller updateInfo
                    }

                else
                    p
            )


type alias EditNoteState =
    { label : String
    , channel : String
    , noteNumber : String
    , velocity : String
    }


defaultEditNoteState : EditNoteState
defaultEditNoteState =
    { label = ""
    , channel = "1"
    , noteNumber = "60"
    , velocity = "100"
    }


updateEditNoteWithMidiMsg : MidiMsg -> EditNoteState -> EditNoteState
updateEditNoteWithMidiMsg midiMsg state =
    case midiMsg of
        NoteOn noteOnParams ->
            { state
              -- Adding 1 to the channel so that they're labelled 1-16
                | channel = String.fromInt (noteOnParams.channel + 1)
                , noteNumber = String.fromInt noteOnParams.pitch
                , velocity = String.fromInt noteOnParams.velocity
            }

        _ ->
            state


editStateToNote : EditNoteState -> Maybe Controller
editStateToNote { label, noteNumber, channel, velocity } =
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
                Controller.newNote label ch nn vel
                    |> Just

            _ ->
                Nothing


type alias EditCCValueState =
    { label : String
    , channel : String
    , controller : String
    , value : String
    }


defaultEditCCValueState : EditCCValueState
defaultEditCCValueState =
    { label = ""
    , channel = "1"
    , controller = "1"
    , value = "63"
    }


updateEditCCValueWithMidiMsg : MidiMsg -> EditCCValueState -> EditCCValueState
updateEditCCValueWithMidiMsg midiMsg state =
    case midiMsg of
        ControllerChange data ->
            { state
              -- Adding 1 to the channel so that they're labelled 1-16
                | channel = String.fromInt (data.channel + 1)
                , controller = String.fromInt data.controller
                , value = String.fromInt data.value
            }

        _ ->
            state


editStateToCCValue : EditCCValueState -> Maybe Controller
editStateToCCValue { label, channel, controller, value } =
    if String.isEmpty label then
        Nothing

    else
        case
            ( Controller.stringToChannel channel
            , String.toInt controller
            , String.toInt value
            )
        of
            ( Just ch, Just c, Just v ) ->
                -- TODO: These values should not exceed 127, handle with midi module
                Controller.newCCValue label ch c v
                    |> Just

            _ ->
                Nothing


type alias EditFaderState =
    { label : String
    , channel : String
    , ccNumber : String
    , valueMin : String
    , valueMax : String
    }


defaultEditFaderState : EditFaderState
defaultEditFaderState =
    { label = ""
    , channel = "1"
    , ccNumber = "1"
    , valueMin = "0"
    , valueMax = "127"
    }


updateEditFaderWithMidiMsg : MidiMsg -> EditFaderState -> EditFaderState
updateEditFaderWithMidiMsg midiMsg state =
    case midiMsg of
        ControllerChange { channel, controller } ->
            { state
              -- Adding 1 to the channel so that they're labelled 1-16
                | channel = String.fromInt (channel + 1)
                , ccNumber = String.fromInt controller
            }

        _ ->
            state


editFaderToFader : EditFaderState -> Maybe Controller
editFaderToFader { label, channel, ccNumber, valueMin, valueMax } =
    if String.isEmpty label then
        Nothing

    else
        case Controller.stringToChannel channel of
            Just ch ->
                case
                    ( String.toInt ccNumber
                    , String.toInt valueMin
                    , String.toInt valueMax
                    )
                of
                    ( Just cc, Just vmin, Just vmax ) ->
                        Controller.Fader
                            { status = Controller.Set
                            , label = label
                            , channel = ch
                            , ccNumber = cc
                            , valuePercent = 50
                            , valueMin = vmin
                            , valueMax = vmax
                            }
                            |> Just

                    _ ->
                        Nothing

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
        |> List.map (\i -> Controller.newNote (String.fromInt i) Controller.Ch7 i 100)
        |> Controller.Row



-- }}}
-- {{{ UPDATE


type Msg
    = MidiDevicesChanged (List Midi.Device)
    | OpenMidiMenu
    | ToggleNormalEdit
    | AddSpace String
    | RemoveItem String
    | OpenEditController String
    | SetEditType EditMenuState
    | UpdateControllerState EditMenuState
    | FinishedEdit Controller
    | ButtonDown String
    | ButtonUp String
    | FaderChanging String Touch.Event
    | FaderSet String
    | ClosePopUp
    | IncomingMidi (Array Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MidiDevicesChanged devices ->
            ( { model | midiStatus = Midi.MidiAvailable devices }
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

                control =
                    Controller.getWithId "0" id page.controller
            in
            ( { model
                | popup =
                    case control of
                        Just (Controller.Row subControls) ->
                            EditRow subControls
                                |> EditMenu id
                                |> Just

                        Just (Controller.Column subControls) ->
                            EditColumn subControls
                                |> EditMenu id
                                |> Just

                        Just (Controller.Note { pitch, label, channel, velocity }) ->
                            EditNote
                                { noteNumber = String.fromInt pitch
                                , label = label
                                , channel = Controller.channelToString channel
                                , velocity = String.fromInt velocity
                                }
                                |> EditMenu id
                                |> Just

                        Just (Controller.CCValue { label, channel, controller, value }) ->
                            EditCCValue
                                { label = label
                                , channel = Controller.channelToString channel
                                , controller = String.fromInt controller
                                , value = String.fromInt value
                                }
                                |> EditMenu id
                                |> Just

                        Just (Controller.Fader { label, channel, ccNumber, valueMin, valueMax }) ->
                            EditFader
                                { label = label
                                , channel = Controller.channelToString channel
                                , ccNumber = String.fromInt ccNumber
                                , valueMin = String.fromInt valueMin
                                , valueMax = String.fromInt valueMax
                                }
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
            in
            case Controller.getWithId "0" id page.controller of
                Just (Controller.Module _ _) ->
                    ( model, Cmd.none )

                Just (Controller.Column _) ->
                    ( model, Cmd.none )

                Just (Controller.Row _) ->
                    ( model, Cmd.none )

                Just (Controller.Note state) ->
                    let
                        note =
                            Controller.Note state

                        ( updatedNote, midiMsg ) =
                            Controller.buttonOn note

                        updatedPage =
                            { page
                                | controller =
                                    Controller.updateWithId
                                        "0"
                                        page.controller
                                        { id = id, updateFn = always updatedNote }
                            }
                    in
                    ( { model | page = updatedPage }
                    , case midiMsg of
                        Just (Midi.NoteOn data) ->
                            Ports.sendNoteOn data

                        _ ->
                            Cmd.none
                    )

                Just (Controller.CCValue state) ->
                    let
                        ccValue =
                            Controller.CCValue state

                        ( updatedCCValue, midiMsg ) =
                            Controller.buttonOn ccValue

                        updatedPage =
                            { page
                                | controller =
                                    Controller.updateWithId
                                        "0"
                                        page.controller
                                        { id = id, updateFn = always updatedCCValue }
                            }
                    in
                    ( { model | page = updatedPage }
                    , case midiMsg of
                        Just (Midi.ControllerChange data) ->
                            Ports.sendCC data

                        _ ->
                            Cmd.none
                    )

                Just (Controller.Fader _) ->
                    ( model, Cmd.none )

                Just Controller.Space ->
                    ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ButtonUp id ->
            let
                page =
                    model.page
            in
            case Controller.getWithId "0" id page.controller of
                Just (Controller.Module _ _) ->
                    ( model, Cmd.none )

                Just (Controller.Column _) ->
                    ( model, Cmd.none )

                Just (Controller.Row _) ->
                    ( model, Cmd.none )

                Just (Controller.Note state) ->
                    let
                        note =
                            Controller.Note state

                        ( updatedNote, midiMsg ) =
                            Controller.buttonOff note

                        updatedPage =
                            { page
                                | controller =
                                    Controller.updateWithId
                                        "0"
                                        page.controller
                                        { id = id, updateFn = always updatedNote }
                            }
                    in
                    ( { model | page = updatedPage }
                    , case midiMsg of
                        Just (Midi.NoteOff data) ->
                            Ports.sendNoteOff data

                        _ ->
                            Cmd.none
                    )

                Just (Controller.CCValue state) ->
                    let
                        ccValue =
                            Controller.CCValue state

                        ( updatedCCValue, _ ) =
                            Controller.buttonOff ccValue

                        updatedPage =
                            { page
                                | controller =
                                    Controller.updateWithId
                                        "0"
                                        page.controller
                                        { id = id, updateFn = always updatedCCValue }
                            }
                    in
                    ( { model | page = updatedPage }
                    , Cmd.none
                    )

                Just (Controller.Fader _) ->
                    ( model, Cmd.none )

                Just Controller.Space ->
                    ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        FaderChanging id touchEvent ->
            let
                touchCoordinates =
                    List.head touchEvent.changedTouches
                        |> Maybe.map .clientPos
                        |> Maybe.withDefault ( 0, 0 )

                identifier =
                    List.head touchEvent.changedTouches
                        |> Maybe.map .identifier
                        |> Maybe.withDefault -1

                fader =
                    Controller.getWithId "0" id page.controller
                        |> Maybe.withDefault
                            (Controller.Fader
                                { status = Controller.Set
                                , label = "ERROR"
                                , channel = Controller.Ch1
                                , ccNumber = 1
                                , valuePercent = 50
                                , valueMin = 0
                                , valueMax = 127
                                }
                            )

                ( newFader, midiMsg ) =
                    Controller.faderChanging identifier touchCoordinates fader

                page =
                    model.page

                updatedPage =
                    { page
                        | controller =
                            Controller.updateWithId
                                "0"
                                page.controller
                                { id = id, updateFn = always newFader }
                    }
            in
            ( { model | page = updatedPage }
            , case midiMsg of
                Just (Midi.ControllerChange data) ->
                    Ports.sendCC data

                _ ->
                    Cmd.none
            )

        FaderSet id ->
            let
                page =
                    model.page

                -- fader =
                --     Controller.getWithId "0" id page.controller
                updatedPage =
                    { page
                        | controller =
                            Controller.updateWithId
                                "0"
                                page.controller
                                { id = id, updateFn = Controller.faderSet }
                    }
            in
            ( { model | page = updatedPage }
            , Cmd.none
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
                [ el
                    [ centerX
                    , width fill
                    , paddingXY 0 12
                    , Background.color <| colourScheme.black
                    , Font.bold
                    , Font.color <| colourScheme.white
                    ]
                    (text "MIDI\nSurf")
                , Input.button
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
                                colourScheme.white

                            Edit False ->
                                colourScheme.lightGrey

                            Edit True ->
                                colourScheme.darkGrey
                    ]
                    { onPress = Just ToggleNormalEdit
                    , label =
                        Icons.edit
                            |> Icons.withSize 36
                            |> Icons.toHtml []
                            |> html
                    }
                ]
            , Lazy.lazy2
                el
                (padding 2 :: fillSpace)
                (renderPage model.mode model.page)
            ]


midiMenu : List Midi.Device -> Element Msg
midiMenu devices =
    el [ centerX, centerY ] <|
        column
            [ padding 10
            , spacing 10
            , Background.color <| colourScheme.white
            , Border.width 4
            ]
            (paragraph [ Font.bold ] [ text "MIDI Devices" ]
                :: (case devices of
                        [] ->
                            [ paragraph [] [ text "No MIDI devices connected." ] ]

                        _ ->
                            [ deviceTable devices ]
                   )
                ++ [ Input.button
                        [ padding 5
                        , Border.width 2
                        , Border.solid
                        , Border.color colourScheme.black
                        ]
                        { onPress = Just ClosePopUp, label = text "Cancel" }
                   ]
            )


deviceTable : List Midi.Device -> Element Msg
deviceTable devices =
    table
        [ padding 10, spacing 10 ]
        { data = devices
        , columns =
            [ { header = el [ Font.underline ] <| Element.text "Name"
              , width = fill
              , view =
                    \{ name } ->
                        Element.text name
              }
            , { header = el [ Font.underline ] <| Element.text "Input"
              , width = fill
              , view =
                    \{ input } ->
                        (case input of
                            Just True ->
                                el [ centerX ] <| Input.defaultCheckbox True

                            _ ->
                                el [ centerX ] <| Input.defaultCheckbox False
                        )
                            |> el [ width fill ]
              }
            , { header = el [ Font.underline ] <| Element.text "Output"
              , width = fill
              , view =
                    \{ output } ->
                        (case output of
                            Just True ->
                                el [ centerX ] <| Input.defaultCheckbox True

                            _ ->
                                el [ centerX ] <| Input.defaultCheckbox False
                        )
                            |> el [ width fill ]
              }
            ]
        }


editMenu : EditMenuState -> Element Msg
editMenu menuType =
    wrappedRow [ centerX, padding 20, spacing 10 ]
        [ el
            [ alignTop
            , padding 10
            , spacing 10
            , Background.color colourScheme.white
            ]
          <|
            Input.radio
                [ spacing 10 ]
                { onChange = SetEditType
                , selected = Just menuType
                , label = Input.labelAbove [ padding 10 ] (text "Type")
                , options =
                    [ Input.option EditModule (text "Module")
                    , Input.option (EditColumn []) (text "Column")
                    , Input.option (EditRow []) (text "Row")
                    , Input.option
                        (case menuType of
                            EditNote _ ->
                                menuType

                            _ ->
                                EditNote defaultEditNoteState
                        )
                        (text "Note")
                    , Input.option
                        (case menuType of
                            EditCCValue _ ->
                                menuType

                            _ ->
                                EditCCValue defaultEditCCValueState
                        )
                        (text "CC Value")
                    , Input.option
                        (case menuType of
                            EditFader _ ->
                                menuType

                            _ ->
                                EditFader defaultEditFaderState
                        )
                        (text "Fader")
                    , Input.option EditSpace (text "Space")
                    ]
                }
        , case menuType of
            EditRow subControls ->
                editRowPane subControls

            EditColumn subControls ->
                editColumnPane subControls

            EditNote state ->
                editNotePane state

            EditCCValue state ->
                editCCValuePane state

            EditFader state ->
                editFaderPane state

            _ ->
                column
                    [ padding 10
                    , spacing 10
                    , Background.color colourScheme.white
                    ]
                    [ Input.button
                        [ padding 5
                        , Border.width 2
                        , Border.solid
                        , Border.color colourScheme.black
                        ]
                        { onPress = Just ClosePopUp, label = text "Cancel" }
                    ]
        ]


editRowPane : List Controller -> Element Msg
editRowPane subControls =
    column
        [ padding 10
        , spacing 10
        , Background.color colourScheme.white
        ]
        (row [ spacing 10 ]
            [ Input.button
                [ padding 5
                , Border.width 2
                , Border.solid
                , Border.color colourScheme.black
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
                , Border.color colourScheme.black
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
                        , Border.color colourScheme.black
                        ]
                        { onPress = Just <| FinishedEdit <| Controller.Row subControls
                        , label = text "Ok"
                        }
                    , Input.button
                        [ padding 5
                        , Border.width 2
                        , Border.solid
                        , Border.color colourScheme.black
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
        , Background.color colourScheme.white
        ]
        (row [ spacing 10 ]
            [ Input.button
                [ padding 5
                , Border.width 2
                , Border.solid
                , Border.color colourScheme.black
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
                , Border.color colourScheme.black
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
                        , Border.color colourScheme.black
                        ]
                        { onPress = Just <| FinishedEdit <| Controller.Column subControls
                        , label = text "Ok"
                        }
                    , Input.button
                        [ padding 5
                        , Border.width 2
                        , Border.solid
                        , Border.color colourScheme.black
                        ]
                        { onPress = Just ClosePopUp, label = text "Cancel" }
                    ]
               ]
        )


editNotePane : EditNoteState -> Element Msg
editNotePane state =
    column
        [ padding 10
        , spacing 10
        , Background.color colourScheme.white
        ]
        [ Input.text
            [ Border.width 2
            , Border.rounded 0
            , Border.color colourScheme.black
            ]
            { onChange =
                \newLabel ->
                    { state | label = newLabel }
                        |> EditNote
                        |> UpdateControllerState
            , text = state.label
            , placeholder = Just <| Input.placeholder [] (text "label")
            , label = Input.labelAbove [] (text "Label")
            }
        , Input.text
            [ Border.width 2
            , Border.rounded 0
            , Border.color colourScheme.black
            ]
            { onChange =
                \newChannel ->
                    { state | channel = newChannel }
                        |> EditNote
                        |> UpdateControllerState
            , text = state.channel
            , placeholder = Just <| Input.placeholder [] (text "channel#")
            , label = Input.labelAbove [] (text "Channel")
            }
        , Input.text
            [ Border.width 2
            , Border.rounded 0
            , Border.color colourScheme.black
            ]
            { onChange =
                \newNoteNumber ->
                    { state | noteNumber = newNoteNumber }
                        |> EditNote
                        |> UpdateControllerState
            , text = state.noteNumber
            , placeholder = Just <| Input.placeholder [] (text "note#")
            , label = Input.labelAbove [] (text "Note Number")
            }
        , Input.text
            [ Border.width 2
            , Border.rounded 0
            , Border.color colourScheme.black
            ]
            { onChange =
                \newVelocity ->
                    { state | velocity = newVelocity }
                        |> EditNote
                        |> UpdateControllerState
            , text = state.velocity
            , placeholder = Just <| Input.placeholder [] (text "velocity")
            , label = Input.labelAbove [] (text "Velocity")
            }
        , row [ spacing 2 ]
            [ case editStateToNote state of
                Just controller ->
                    Input.button
                        [ padding 5
                        , Border.width 2
                        , Border.solid
                        , Border.color colourScheme.black
                        ]
                        { onPress = Just <| FinishedEdit controller
                        , label = text "Ok"
                        }

                Nothing ->
                    Input.button
                        [ padding 5
                        , Border.width 2
                        , Border.solid
                        , Border.color colourScheme.lightGrey
                        , Font.color colourScheme.lightGrey
                        ]
                        { onPress = Nothing
                        , label = text "Ok"
                        }
            , Input.button
                [ padding 5
                , Border.width 2
                , Border.solid
                , Border.color colourScheme.black
                ]
                { onPress = Just ClosePopUp, label = text "Cancel" }
            ]
        ]


editCCValuePane : EditCCValueState -> Element Msg
editCCValuePane state =
    column
        [ padding 10
        , spacing 10
        , Background.color colourScheme.white
        ]
        [ Input.text
            [ Border.width 2
            , Border.rounded 0
            , Border.color colourScheme.black
            ]
            { onChange =
                \newLabel ->
                    { state | label = newLabel }
                        |> EditCCValue
                        |> UpdateControllerState
            , text = state.label
            , placeholder = Just <| Input.placeholder [] (text "label")
            , label = Input.labelAbove [] (text "Label")
            }
        , Input.text
            [ Border.width 2
            , Border.rounded 0
            , Border.color colourScheme.black
            ]
            { onChange =
                \newChannel ->
                    { state | channel = newChannel }
                        |> EditCCValue
                        |> UpdateControllerState
            , text = state.channel
            , placeholder = Just <| Input.placeholder [] (text "channel#")
            , label = Input.labelAbove [] (text "Channel")
            }
        , Input.text
            [ Border.width 2
            , Border.rounded 0
            , Border.color colourScheme.black
            ]
            { onChange =
                \newController ->
                    { state | controller = newController }
                        |> EditCCValue
                        |> UpdateControllerState
            , text = state.controller
            , placeholder = Just <| Input.placeholder [] (text "controller#")
            , label = Input.labelAbove [] (text "Controller Number")
            }
        , Input.text
            [ Border.width 2
            , Border.rounded 0
            , Border.color colourScheme.black
            ]
            { onChange =
                \newValue ->
                    { state | value = newValue }
                        |> EditCCValue
                        |> UpdateControllerState
            , text = state.value
            , placeholder = Just <| Input.placeholder [] (text "value")
            , label = Input.labelAbove [] (text "Value")
            }
        , row [ spacing 2 ]
            [ case editStateToCCValue state of
                Just controller ->
                    Input.button
                        [ padding 5
                        , Border.width 2
                        , Border.solid
                        , Border.color colourScheme.black
                        ]
                        { onPress = Just <| FinishedEdit controller
                        , label = text "Ok"
                        }

                Nothing ->
                    Input.button
                        [ padding 5
                        , Border.width 2
                        , Border.solid
                        , Border.color colourScheme.lightGrey
                        , Font.color colourScheme.lightGrey
                        ]
                        { onPress = Nothing
                        , label = text "Ok"
                        }
            , Input.button
                [ padding 5
                , Border.width 2
                , Border.solid
                , Border.color colourScheme.black
                ]
                { onPress = Just ClosePopUp, label = text "Cancel" }
            ]
        ]


editFaderPane : EditFaderState -> Element Msg
editFaderPane state =
    column
        [ padding 10
        , spacing 10
        , Background.color colourScheme.white
        ]
        [ Input.text
            [ Border.width 2
            , Border.rounded 0
            , Border.color colourScheme.black
            ]
            { onChange =
                \newLabel ->
                    { state | label = newLabel }
                        |> EditFader
                        |> UpdateControllerState
            , text = state.label
            , placeholder = Just <| Input.placeholder [] (text "label")
            , label = Input.labelAbove [] (text "Label")
            }
        , Input.text
            [ Border.width 2
            , Border.rounded 0
            , Border.color colourScheme.black
            ]
            { onChange =
                \newChannel ->
                    { state | channel = newChannel }
                        |> EditFader
                        |> UpdateControllerState
            , text = state.channel
            , placeholder = Just <| Input.placeholder [] (text "channel#")
            , label = Input.labelAbove [] (text "Channel")
            }
        , Input.text
            [ Border.width 2
            , Border.rounded 0
            , Border.color colourScheme.black
            ]
            { onChange =
                \newCCNumber ->
                    { state | ccNumber = newCCNumber }
                        |> EditFader
                        |> UpdateControllerState
            , text = state.ccNumber
            , placeholder = Just <| Input.placeholder [] (text "cc#")
            , label = Input.labelAbove [] (text "CC Number")
            }
        , Input.text
            [ Border.width 2
            , Border.rounded 0
            , Border.color colourScheme.black
            ]
            { onChange =
                \newMinValue ->
                    { state | valueMin = newMinValue }
                        |> EditFader
                        |> UpdateControllerState
            , text = state.valueMin
            , placeholder = Just <| Input.placeholder [] (text "min value")
            , label = Input.labelAbove [] (text "Min Value")
            }
        , Input.text
            [ Border.width 2
            , Border.rounded 0
            , Border.color colourScheme.black
            ]
            { onChange =
                \newMaxValue ->
                    { state | valueMax = newMaxValue }
                        |> EditFader
                        |> UpdateControllerState
            , text = state.valueMax
            , placeholder = Just <| Input.placeholder [] (text "max value")
            , label = Input.labelAbove [] (text "Max Value")
            }
        , row [ spacing 2 ]
            [ case editFaderToFader state of
                Just controller ->
                    Input.button
                        [ padding 5
                        , Border.width 2
                        , Border.solid
                        , Border.color colourScheme.black
                        ]
                        { onPress = Just <| FinishedEdit controller
                        , label = text "Ok"
                        }

                Nothing ->
                    Input.button
                        [ padding 5
                        , Border.width 2
                        , Border.solid
                        , Border.color colourScheme.lightGrey
                        , Font.color colourScheme.lightGrey
                        ]
                        { onPress = Nothing
                        , label = text "Ok"
                        }
            , Input.button
                [ padding 5
                , Border.width 2
                , Border.solid
                , Border.color colourScheme.black
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

        Controller.Note state ->
            renderNote
                config
                mode
                state
                (updatedParts
                    |> List.reverse
                    |> String.join "_"
                )

        Controller.CCValue state ->
            renderCCValue
                config
                mode
                state
                (updatedParts
                    |> List.reverse
                    |> String.join "_"
                )

        Controller.Fader state ->
            renderFader
                config
                mode
                state
                (updatedParts
                    |> List.reverse
                    |> String.join "_"
                )

        Controller.Space ->
            case mode of
                Normal ->
                    el
                        (Background.color colourScheme.lightGrey
                            :: fillSpace
                        )
                        none

                Edit _ ->
                    el
                        ([ Background.color colourScheme.lightGrey
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


renderNote : PageConfig -> Mode -> Controller.NoteState -> String -> Element Msg
renderNote config mode state id =
    case mode of
        Normal ->
            el
                ([ padding 0
                 , spacing 0
                 , Border.width 4
                 , case state.status of
                    Controller.Off ->
                        if modBy 12 state.pitch == 0 then
                            Background.color <| colourScheme.blue

                        else if List.member (modBy 12 state.pitch) [ 1, 3, 6, 8, 10 ] then
                            Background.color <| colourScheme.darkGrey

                        else
                            Background.color <| colourScheme.lightGrey

                    Controller.On ->
                        Border.dashed
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
                 , if modBy 12 state.pitch == 0 then
                    Background.color colourScheme.blue

                   else if List.member (modBy 12 state.pitch) [ 1, 3, 6, 8, 10 ] then
                    Background.color colourScheme.darkGrey

                   else
                    Background.color colourScheme.lightGrey
                 ]
                    ++ fillSpace
                )
                { onPress = Just <| OpenEditController id
                , label =
                    state.label
                        |> text
                }


renderCCValue : PageConfig -> Mode -> Controller.CCValueState -> String -> Element Msg
renderCCValue config mode state id =
    case mode of
        Normal ->
            el
                ([ padding 0
                 , spacing 0
                 , Border.width 4
                 , case state.status of
                    Controller.Off ->
                        Background.color <| colourScheme.green

                    Controller.On ->
                        Border.dashed
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
                 , Background.color colourScheme.green
                 ]
                    ++ fillSpace
                )
                { onPress = Just <| OpenEditController id
                , label =
                    state.label
                        |> text
                }


renderFader : PageConfig -> Mode -> Controller.FaderState -> String -> Element Msg
renderFader config mode state id =
    case mode of
        Normal ->
            el
                ([ padding 0
                 , spacing 0
                 , Border.width 4
                 , case state.status of
                    Controller.Set ->
                        Background.color <| colourScheme.yellow

                    Controller.Changing _ _ ->
                        Border.dashed
                 , htmlAttribute <|
                    Touch.onStart
                        (\event ->
                            FaderChanging id event
                        )

                 -- , htmlAttribute <|
                 --    Mouse.onDown
                 --        (\_ ->
                 --            FaderChanging id
                 --        )
                 , htmlAttribute <|
                    Touch.onMove
                        (\event ->
                            FaderChanging id event
                        )
                 , htmlAttribute <|
                    Touch.onEnd
                        (\_ ->
                            FaderSet id
                        )
                 , htmlAttribute <|
                    Mouse.onUp
                        (\_ ->
                            FaderSet id
                        )
                 ]
                    ++ fillSpace
                )
                (column
                    (Background.color colourScheme.white :: fillSpace)
                    [ column
                        fillSpace
                        [ el
                            [ height <| fillPortion (100 - state.valuePercent)
                            , width fill
                            , Background.color colourScheme.lightGrey
                            ]
                            none
                        , el
                            [ height <| fillPortion state.valuePercent
                            , width fill
                            , Background.color colourScheme.yellow
                            , Border.widthEach { bottom = 0, top = 4, left = 0, right = 0 }
                            ]
                            none
                        ]
                    , el [ centerX, padding 10 ] <| text state.label
                    ]
                )

        Edit _ ->
            Input.button
                ([ padding config.gapSize
                 , spacing config.gapSize
                 , Border.width 2
                 , Border.dashed
                 , Font.size 14
                 , Background.color colourScheme.yellow
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


colourScheme :
    { black : Color
    , darkGrey : Color
    , lightGrey : Color
    , white : Color
    , green : Color
    , blue : Color
    , yellow : Color
    , red : Color
    }
colourScheme =
    { white = rgb255 255 255 255
    , lightGrey = rgb255 200 200 200
    , darkGrey = rgb255 86 90 94
    , black = rgb255 0 0 0
    , green = rgb255 0 133 86
    , blue = rgb255 35 141 193
    , yellow = rgb255 255 183 27
    , red = rgb255 202 0 61
    }



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
