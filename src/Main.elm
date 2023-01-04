module Main exposing (..)

import Array exposing (Array)
import Browser
import Codec exposing (Codec, Value)
import Controller exposing (Controller, FaderStatus(..))
import Dict
import EditableController as EController exposing (EditableController(..))
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import Element.Region as Region
import FeatherIcons as Icons
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Midi exposing (MidiMsg(..), Status(..))
import Ports
import Style exposing (..)


version : String
version =
    "0.1.0"



-- {{{ MODEL


type alias Model =
    { midiStatus : Status
    , mode : Mode
    , pages : Array Page
    , activePage : Int
    , menuOpen : Bool
    , popup : Maybe PopUp
    , midiLog : List String
    }


modelCodec : Codec Model
modelCodec =
    Codec.object Model
        |> Codec.field "midiStatus" .midiStatus (Codec.constant Midi.Initialising)
        |> Codec.field "mode" .mode (Codec.constant Normal)
        |> Codec.field "pages" .pages (Codec.array pageCodec)
        |> Codec.field "activePage" .activePage (Codec.constant 0)
        |> Codec.field "menuOpen" .menuOpen (Codec.constant True)
        |> Codec.field "popup" .popup (Codec.constant Nothing)
        |> Codec.field "midiLog" .midiLog (Codec.constant [])
        |> Codec.buildObject


type Mode
    = Normal
    | Edit Bool


type PopUp
    = InfoPanel
    | MidiMenu
    | SaveMenu
    | EditMenu String EditableController
    | NewPageMenu PageMenuState
    | EditPageMenu Int PageMenuState


type alias PageMenuState =
    { label : String
    }


type alias Page =
    { label : String
    , controller : Controller
    , config : PageConfig
    }


pageCodec : Codec Page
pageCodec =
    Codec.object Page
        |> Codec.field "label" .label Codec.string
        |> Codec.field "controller" .controller Controller.controllerCodec
        |> Codec.field "config" .config pageConfigCodec
        |> Codec.buildObject


type alias PageConfig =
    { gapSize : Int
    , debug : Bool
    }


pageConfigCodec : Codec PageConfig
pageConfigCodec =
    Codec.object PageConfig
        |> Codec.field "gapSize" .gapSize Codec.int
        |> Codec.field "debug" .debug Codec.bool
        |> Codec.buildObject


getControllerFromActivePage : String -> Int -> Array Page -> Maybe Controller
getControllerFromActivePage id activePage pages =
    pages
        |> Array.get activePage
        |> Maybe.map .controller
        |> Maybe.andThen (Controller.getWithId "0" id)


updateControllerOnActivePage :
    Int
    -> { id : String, updateFn : Controller -> Controller }
    -> Array Page
    -> Array Page
updateControllerOnActivePage activePage updateInfo pages =
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


type alias Flags =
    { mInitialState : Value }


init : Flags -> ( Model, Cmd Msg )
init { mInitialState } =
    case Codec.decodeValue (Codec.maybe modelCodec) mInitialState of
        Ok (Just model) ->
            ( model, Cmd.none )

        _ ->
            ( { midiStatus = Midi.Initialising
              , mode = Normal
              , pages = Array.fromList <| List.repeat 4 defaultPage
              , activePage = 0
              , menuOpen = True
              , popup = Just <| InfoPanel
              , midiLog = []
              }
            , Cmd.none
            )


defaultPage : Page
defaultPage =
    { label = "1"
    , controller =
        isomorphicKeyboard
            { channel = Midi.Ch6
            , velocity = 100
            , firstNote = 36
            , numberOfRows = 12
            , offset = 5
            , rowLength = 18
            }
    , config =
        { gapSize = 2
        , debug = True
        }
    }


isomorphicKeyboard :
    { channel : Midi.Channel
    , velocity : Int
    , firstNote : Int
    , numberOfRows : Int
    , offset : Int
    , rowLength : Int
    }
    -> Controller
isomorphicKeyboard { channel, velocity, firstNote, numberOfRows, offset, rowLength } =
    let
        noteRange =
            List.range firstNote 127

        rowNumbers =
            List.range 0 (numberOfRows - 1)
    in
    List.map (makeIsomorphicRow channel velocity noteRange offset (rowLength - 1)) rowNumbers
        |> List.reverse
        |> Controller.Column


makeIsomorphicRow : Midi.Channel -> Int -> List Int -> Int -> Int -> Int -> Controller
makeIsomorphicRow channel velocity noteRange offset rowLength rowNumber =
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
        |> List.map
            (\i ->
                Controller.newNote (String.fromInt i) (Style.pitchToAppColour i) channel i velocity
            )
        |> Controller.Row



-- }}}
-- {{{ UPDATE


type Msg
    = MidiDevicesChanged (List Midi.Device)
    | ToggleMenu
    | OpenInfoPanel
    | OpenMidiMenu
    | OpenSaveLoadMenu
    | ToggleNormalEdit
    | OpenEditPageMenu Int
    | DeletePage Int
    | OpenNewPageMenu
    | UpdatePageMenuState PageMenuState
    | AddPage PageMenuState
    | UpdatePage Int PageMenuState
    | AddSpace String
    | RemoveItem String
    | OpenEditController String
    | SetEditType EditableController
    | UpdateControllerState EditableController
    | FinishedEdit Controller
    | SelectActivePage Int
    | ButtonDown String
    | ButtonUp String
    | FaderChanging String Touch.Event
    | FaderChangingMouse String Mouse.Event
    | FaderSet String
    | ClosePopUp
    | IncomingMidi { deviceName : String, midiData : Array Int }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MidiDevicesChanged devices ->
            ( { model | midiStatus = Midi.MidiAvailable devices }
            , Cmd.none
            )

        ToggleMenu ->
            ( { model | menuOpen = not model.menuOpen }
            , Cmd.none
            )

        OpenInfoPanel ->
            ( { model
                | popup =
                    Just <|
                        InfoPanel
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

        OpenSaveLoadMenu ->
            ( { model
                | popup =
                    Just <|
                        SaveMenu
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

        OpenEditPageMenu index ->
            let
                mSelectedPage =
                    Array.get index model.pages
            in
            case mSelectedPage of
                Just page ->
                    ( { model
                        | popup =
                            Just <|
                                EditPageMenu index { label = page.label }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeletePage index ->
            let
                newModel =
                    { model
                        | pages =
                            model.pages
                                |> Array.indexedMap Tuple.pair
                                |> Array.filter (\( i, _ ) -> i /= index)
                                |> Array.map Tuple.second
                        , popup = Nothing
                    }
            in
            ( newModel
            , Ports.saveState <|
                Codec.encodeToValue modelCodec newModel
            )

        OpenNewPageMenu ->
            ( { model
                | popup =
                    Just <|
                        NewPageMenu { label = "" }
              }
            , Cmd.none
            )

        UpdatePageMenuState state ->
            case model.popup of
                Just (NewPageMenu _) ->
                    ( { model
                        | popup =
                            Just <|
                                NewPageMenu state
                      }
                    , Cmd.none
                    )

                Just (EditPageMenu index _) ->
                    ( { model
                        | popup =
                            Just <|
                                EditPageMenu index state
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model
                        | popup = Nothing
                      }
                    , Cmd.none
                    )

        UpdatePage index state ->
            let
                oldPage =
                    model.pages
                        |> Array.get index

                newModel =
                    case oldPage of
                        Just page ->
                            { model
                                | pages =
                                    Array.set
                                        index
                                        { page | label = state.label }
                                        model.pages

                                -- I don't need to subtract 1 for zero indexing
                                -- as this is the old array length
                                , activePage = index
                                , popup = Nothing
                            }

                        Nothing ->
                            { model
                                | popup = Nothing
                            }
            in
            ( newModel
            , Ports.saveState <|
                Codec.encodeToValue modelCodec newModel
            )

        AddPage state ->
            let
                newModel =
                    { model
                        | pages =
                            Array.push
                                { label = state.label
                                , controller = Controller.Space
                                , config =
                                    { gapSize = 2
                                    , debug = True
                                    }
                                }
                                model.pages

                        -- I don't need to subtract 1 for zero indexing
                        -- as this is the old array length
                        , activePage = Array.length model.pages
                        , popup = Nothing
                    }
            in
            ( newModel
            , Ports.saveState <|
                Codec.encodeToValue modelCodec newModel
            )

        AddSpace id ->
            ( { model
                | pages =
                    updateControllerOnActivePage
                        model.activePage
                        { id = id, updateFn = Controller.addSpace }
                        model.pages
              }
            , Cmd.none
            )

        RemoveItem id ->
            ( { model
                | pages =
                    updateControllerOnActivePage
                        model.activePage
                        { id = id, updateFn = Controller.removeItem }
                        model.pages
              }
            , Cmd.none
            )

        OpenEditController id ->
            let
                control =
                    getControllerFromActivePage id model.activePage model.pages
            in
            ( { model
                | popup =
                    case control of
                        Just (Controller.Module label subController) ->
                            EditModule label subController
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

                        Just (Controller.Note { label, colour, pitch, channel, velocity }) ->
                            EditNote
                                { label = label
                                , colour = colour
                                , pitch = String.fromInt pitch
                                , channel = Midi.channelToString channel
                                , velocity = String.fromInt velocity
                                }
                                |> EditMenu id
                                |> Just

                        Just (Controller.Chord { label, colour, velocity, notes }) ->
                            EditChord
                                { label = label
                                , colour = colour
                                , velocity = String.fromInt velocity
                                , notes =
                                    notes
                                        |> List.map
                                            (\({ channel, pitch } as note) ->
                                                ( ( Midi.channelToMidiNumber channel
                                                  , pitch
                                                  )
                                                , note
                                                )
                                            )
                                        |> Dict.fromList
                                }
                                |> EditMenu id
                                |> Just

                        Just (Controller.CCValue { label, colour, channel, controller, value }) ->
                            EditCCValue
                                { label = label
                                , colour = colour
                                , channel = Midi.channelToString channel
                                , controller = String.fromInt controller
                                , value = String.fromInt value
                                }
                                |> EditMenu id
                                |> Just

                        Just (Controller.Fader state) ->
                            EditFader
                                { label = state.label
                                , colour = state.colour
                                , channel = Midi.channelToString state.channel
                                , ccNumber = String.fromInt state.ccNumber
                                , valueMin = String.fromInt state.valueMin
                                , valueMax = String.fromInt state.valueMax
                                }
                                |> EditMenu id
                                |> Just

                        Just Controller.MidiLog ->
                            EditMidiLog
                                |> EditMenu id
                                |> Just

                        Just Controller.Space ->
                            EditSpace
                                |> EditMenu id
                                |> Just

                        Nothing ->
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
                        newModel =
                            { model
                                | popup = Nothing
                                , pages =
                                    updateControllerOnActivePage
                                        model.activePage
                                        { id = id, updateFn = \_ -> controller }
                                        model.pages
                                , mode =
                                    case model.mode of
                                        Normal ->
                                            Normal

                                        Edit False ->
                                            Normal

                                        Edit True ->
                                            Edit True
                            }
                    in
                    ( newModel
                    , Ports.saveState <|
                        Codec.encodeToValue modelCodec newModel
                    )

                _ ->
                    ( { model
                        | popup = Nothing
                        , mode = Normal
                      }
                    , Cmd.none
                    )

        SelectActivePage activePage ->
            ( { model | activePage = activePage }
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
            case getControllerFromActivePage id model.activePage model.pages of
                Just (Controller.Module _ _) ->
                    ( model, Cmd.none )

                Just (Controller.Column _) ->
                    ( model, Cmd.none )

                Just (Controller.Row _) ->
                    ( model, Cmd.none )

                Just ((Controller.Note _) as note) ->
                    let
                        ( updatedNote, midiMsgs ) =
                            Controller.buttonOn note
                    in
                    ( { model
                        | pages =
                            updateControllerOnActivePage
                                model.activePage
                                { id = id, updateFn = always updatedNote }
                                model.pages
                      }
                    , List.map Ports.midiMsgToCmd midiMsgs
                        |> Cmd.batch
                    )

                Just ((Controller.Chord _) as chord) ->
                    let
                        ( updatedChord, midiMsgs ) =
                            Controller.buttonOn chord
                    in
                    ( { model
                        | pages =
                            updateControllerOnActivePage
                                model.activePage
                                { id = id, updateFn = always updatedChord }
                                model.pages
                      }
                    , List.map Ports.midiMsgToCmd midiMsgs
                        |> Cmd.batch
                    )

                Just ((Controller.CCValue _) as ccValue) ->
                    let
                        ( updatedCCValue, midiMsgs ) =
                            Controller.buttonOn ccValue
                    in
                    ( { model
                        | pages =
                            updateControllerOnActivePage
                                model.activePage
                                { id = id, updateFn = always updatedCCValue }
                                model.pages
                      }
                    , List.map Ports.midiMsgToCmd midiMsgs
                        |> Cmd.batch
                    )

                Just (Controller.Fader _) ->
                    ( model, Cmd.none )

                Just Controller.MidiLog ->
                    ( model, Cmd.none )

                Just Controller.Space ->
                    ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ButtonUp id ->
            case getControllerFromActivePage id model.activePage model.pages of
                Just (Controller.Module _ _) ->
                    ( model, Cmd.none )

                Just (Controller.Column _) ->
                    ( model, Cmd.none )

                Just (Controller.Row _) ->
                    ( model, Cmd.none )

                Just ((Controller.Note _) as note) ->
                    let
                        ( updatedNote, midiMsgs ) =
                            Controller.buttonOff note
                    in
                    ( { model
                        | pages =
                            updateControllerOnActivePage
                                model.activePage
                                { id = id, updateFn = always updatedNote }
                                model.pages
                      }
                    , List.map Ports.midiMsgToCmd midiMsgs
                        |> Cmd.batch
                    )

                Just ((Controller.Chord _) as chord) ->
                    let
                        ( updatedChord, midiMsgs ) =
                            Controller.buttonOff chord
                    in
                    ( { model
                        | pages =
                            updateControllerOnActivePage
                                model.activePage
                                { id = id, updateFn = always updatedChord }
                                model.pages
                      }
                    , List.map Ports.midiMsgToCmd midiMsgs
                        |> Cmd.batch
                    )

                Just ((Controller.CCValue _) as ccValue) ->
                    let
                        ( updatedCCValue, _ ) =
                            Controller.buttonOff ccValue
                    in
                    ( { model
                        | pages =
                            updateControllerOnActivePage
                                model.activePage
                                { id = id, updateFn = always updatedCCValue }
                                model.pages
                      }
                    , Cmd.none
                    )

                Just (Controller.Fader _) ->
                    ( model, Cmd.none )

                Just Controller.MidiLog ->
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
                    getControllerFromActivePage id model.activePage model.pages
                        |> Maybe.withDefault
                            (Controller.Fader
                                { status = Controller.Set
                                , label = "ERROR"
                                , colour = LightGrey
                                , channel = Midi.Ch1
                                , ccNumber = 1
                                , valuePercent = 50
                                , valueMin = 0
                                , valueMax = 127
                                }
                            )

                ( newFader, midiMsg ) =
                    Controller.faderChanging identifier touchCoordinates fader
            in
            ( { model
                | pages =
                    updateControllerOnActivePage
                        model.activePage
                        { id = id, updateFn = always newFader }
                        model.pages
              }
            , case midiMsg of
                Just (Midi.ControllerChange data) ->
                    Ports.sendCC data

                _ ->
                    Cmd.none
            )

        FaderChangingMouse id mouseEvent ->
            let
                fader =
                    getControllerFromActivePage id model.activePage model.pages
                        |> Maybe.withDefault
                            (Controller.Fader
                                { status = Controller.Set
                                , label = "ERROR"
                                , colour = LightGrey
                                , channel = Midi.Ch1
                                , ccNumber = 1
                                , valuePercent = 50
                                , valueMin = 0
                                , valueMax = 127
                                }
                            )

                ( newFader, midiMsg ) =
                    Controller.faderChanging -1 mouseEvent.clientPos fader
            in
            ( { model
                | pages =
                    updateControllerOnActivePage
                        model.activePage
                        { id = id, updateFn = always newFader }
                        model.pages
              }
            , case midiMsg of
                Just (Midi.ControllerChange data) ->
                    Ports.sendCC data

                _ ->
                    Cmd.none
            )

        FaderSet id ->
            ( { model
                | pages =
                    updateControllerOnActivePage
                        model.activePage
                        { id = id, updateFn = Controller.faderSet }
                        model.pages
              }
            , Cmd.none
            )

        ClosePopUp ->
            ( { model | popup = Nothing }
            , Cmd.none
            )

        IncomingMidi { deviceName, midiData } ->
            let
                midiMsg =
                    Midi.intArrayToMidiMsg midiData
            in
            ( { model
                | popup =
                    case model.popup of
                        Just (EditMenu id state) ->
                            EController.updateWithMidiMsg midiMsg state
                                |> EditMenu id
                                |> Just

                        _ ->
                            model.popup
                , midiLog =
                    (deviceName
                        ++ " "
                        ++ Midi.midiMsgToString midiMsg
                    )
                        :: List.take 199 model.midiLog
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
                    renderPopup model.midiStatus popup
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
        (column
            fillSpace
            [ titleBar model.mode model.menuOpen model.activePage model.pages
            , case Array.get model.activePage model.pages of
                Just page ->
                    Lazy.lazy2
                        el
                        ([ padding 2, scrollbars ] ++ fillSpace)
                        (renderPage model.mode model.midiLog page)

                Nothing ->
                    el fillSpace <|
                        el [ centerX, centerY ] (text "No page selected.")
            ]
        )



-- {{{ Title Bar


titleBar : Mode -> Bool -> Int -> Array Page -> Element Msg
titleBar mode menuOpen activePage pages =
    row
        [ height <| px 76
        , width fill
        , padding 4
        , spacing 4
        , scrollbars
        , Border.widthEach { bottom = 4, top = 0, left = 0, right = 0 }
        ]
        [ column
            [ paddingXY 8 12
            , backgroundColour Black
            , Font.bold
            , fontColour White
            , Region.heading 1
            ]
            [ text "MIDI"
            , text "Surf"
            ]
        , menuRow mode menuOpen
        , row
            [ alignRight
            , height fill
            , spacing 4
            ]
            ((Array.indexedMap (pageButton mode activePage) pages
                |> Array.toList
             )
                ++ (case mode of
                        Normal ->
                            []

                        Edit _ ->
                            [ newPageButton ]
                   )
            )
        ]


menuRow : Mode -> Bool -> Element Msg
menuRow mode menuOpen =
    row
        [ height fill ]
        (Input.button
            [ padding 10
            , if menuOpen then
                borderColour LightGrey

              else
                borderColour Black
            , Border.width 4
            , if menuOpen then
                backgroundColour LightGrey

              else
                backgroundColour White
            ]
            { onPress = Just ToggleMenu
            , label =
                Icons.menu
                    |> Icons.withSize 36
                    |> Icons.toHtml []
                    |> html
            }
            :: (if menuOpen then
                    [ row
                        [ alignLeft
                        , height fill
                        , spacing 4
                        , paddingXY 4 0
                        , backgroundColour LightGrey
                        ]
                        [ Input.button
                            [ padding 10
                            , Border.width 4
                            , backgroundColour White
                            ]
                            { onPress = Just OpenInfoPanel
                            , label =
                                Icons.info
                                    |> Icons.withSize 28
                                    |> Icons.toHtml []
                                    |> html
                            }
                        , Input.button
                            [ padding 10
                            , Border.width 4
                            , backgroundColour White
                            ]
                            { onPress = Just OpenMidiMenu
                            , label =
                                Icons.gitPullRequest
                                    |> Icons.withSize 28
                                    |> Icons.toHtml []
                                    |> html
                            }
                        , Input.button
                            [ padding 10
                            , Border.width 4
                            , backgroundColour White
                            ]
                            { onPress = Just OpenSaveLoadMenu
                            , label =
                                Icons.save
                                    |> Icons.withSize 28
                                    |> Icons.toHtml []
                                    |> html
                            }
                        , Input.button
                            [ padding 10
                            , Border.width 4
                            , case mode of
                                Normal ->
                                    backgroundColour White

                                Edit False ->
                                    backgroundColour LightGrey

                                Edit True ->
                                    backgroundColour DarkGrey
                            ]
                            { onPress = Just ToggleNormalEdit
                            , label =
                                Icons.edit
                                    |> Icons.withSize 28
                                    |> Icons.toHtml []
                                    |> html
                            }
                        ]
                    ]

                else
                    []
               )
        )


pageButton : Mode -> Int -> Int -> Page -> Element Msg
pageButton mode activePage pageNumber { label } =
    Input.button
        ([ padding 10
         , height fill
         ]
            ++ (case mode of
                    Normal ->
                        []

                    Edit _ ->
                        [ Border.width 4
                        , borderColour Black
                        , Border.dashed
                        ]
               )
            ++ (if pageNumber == activePage then
                    [ backgroundColour Black
                    , fontColour White
                    ]

                else
                    [ backgroundColour LightGrey
                    , fontColour Black
                    ]
               )
        )
        { onPress =
            case mode of
                Normal ->
                    Just (SelectActivePage pageNumber)

                Edit _ ->
                    Just (OpenEditPageMenu pageNumber)
        , label = text label
        }


newPageButton : Element Msg
newPageButton =
    Input.button
        [ padding 10
        , height fill
        , Border.width 4
        ]
        { onPress = Just OpenNewPageMenu
        , label =
            Icons.plus
                |> Icons.withSize 36
                |> Icons.toHtml []
                |> html
        }



-- }}}
-- {{{ Popups


renderPopup : Midi.Status -> PopUp -> Element Msg
renderPopup midiStatus popup =
    el
        ([ padding 5, Background.color (rgba 0.5 0.5 0.5 0.8) ]
            ++ fillSpace
        )
        (case popup of
            InfoPanel ->
                infoPanel

            MidiMenu ->
                case midiStatus of
                    Midi.MidiAvailable devices ->
                        midiMenu devices

                    _ ->
                        midiMenu []

            SaveMenu ->
                saveMenu

            EditMenu _ state ->
                editMenu state

            NewPageMenu state ->
                newPageMenu state

            EditPageMenu index state ->
                editPageMenu index state
        )



-- {{{ Info Panel


infoPanel : Element Msg
infoPanel =
    el [ centerX, centerY, height fill ] <|
        column
            [ centerX
            , height fill
            , padding 20
            , spacing 20
            , backgroundColour White
            , Border.width 4
            ]
            [ column
                [ spacing 20, height fill, scrollbarY ]
                [ column
                    [ centerX
                    , paddingXY 8 18
                    , backgroundColour Black
                    , Font.bold
                    , Font.size 48
                    , fontColour White
                    , Region.heading 2
                    ]
                    [ text "MIDI"
                    , text "Surf"
                    ]
                , paragraph [] [ text "Version ", text version ]
                , paragraph []
                    [ """Thank you for using MIDI Surf! If you find it useful,
                      please consider supporting the development """
                        |> text
                    , newTabLink
                        linkStyle
                        { url = "https://patreon.com/user?u=85350251"
                        , label = text "on Patreon"
                        }
                    , """, where you can request new features.
                      Check out our channel
                      """ |> text
                    , newTabLink
                        linkStyle
                        { url = "https://www.youtube.com/@mochreach"
                        , label = text "on YouTube"
                        }
                    , """ for more videos about MIDI Surf and other music
                      technology topics.
                      """ |> text
                    ]
                , paragraph []
                    [ text "Copyright: "
                    , newTabLink
                        linkStyle
                        { url = "https://mochreach.dev"
                        , label = text "Mo Chreach! Music Technology Ltd"
                        }
                    ]
                ]
            , Input.button
                [ padding 5
                , centerX
                , Border.width 2
                , Border.solid
                , borderColour Black
                ]
                { onPress = Just ClosePopUp, label = text "Close" }
            ]



-- }}}
-- {{{ Midi Menu


midiMenu : List Midi.Device -> Element Msg
midiMenu devices =
    el [ centerX, centerY ] <|
        column
            [ padding 10
            , spacing 10
            , backgroundColour White
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
                        , borderColour Black
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



-- }}}
-- {{{ Save Menu


saveMenu : Element Msg
saveMenu =
    el [ centerX, centerY ] <|
        column
            [ padding 10
            , spacing 10
            , backgroundColour White
            , Border.width 4
            ]
            [ paragraph [ Font.bold ] [ text "Save/Load" ]
            , Input.button
                [ padding 5
                , Border.width 2
                , Border.solid
                , borderColour Black
                ]
                { onPress = Just ClosePopUp, label = text "Cancel" }
            ]



-- }}}
-- {{{ Edit Menu


editMenu : EditableController -> Element Msg
editMenu menuType =
    row [ padding 20, spacing 10, width fill, height fill ]
        [ el
            [ alignTop
            , padding 10
            , spacing 10
            , backgroundColour White
            , Border.width 4
            ]
          <|
            Input.radio
                [ spacing 10 ]
                { onChange = SetEditType
                , selected = Just menuType
                , label = Input.labelHidden "Type Selector"
                , options =
                    [ Input.option (EditModule "" Controller.Space) (text "Module")
                    , Input.option
                        (EditIsomorphic EController.defaultEditIsomorphicState)
                        (text "Isomorphic")
                    , Input.option (EditColumn []) (text "Column")
                    , Input.option (EditRow []) (text "Row")
                    , Input.option
                        (case menuType of
                            EditNote _ ->
                                menuType

                            _ ->
                                EditNote EController.defaultEditNoteState
                        )
                        (text "Note")
                    , Input.option
                        (case menuType of
                            EditChord _ ->
                                menuType

                            _ ->
                                EditChord EController.defaultEditChordState
                        )
                        (text "Chord")
                    , Input.option
                        (case menuType of
                            EditCCValue _ ->
                                menuType

                            _ ->
                                EditCCValue EController.defaultEditCCValueState
                        )
                        (text "CC Value")
                    , Input.option
                        (case menuType of
                            EditFader _ ->
                                menuType

                            _ ->
                                EditFader EController.defaultEditFaderState
                        )
                        (text "Fader")
                    , Input.option EditMidiLog (text "MIDI Log")
                    , Input.option EditSpace (text "Space")
                    ]
                }
        , case menuType of
            EditModule label subController ->
                editModulePane label subController

            EditIsomorphic state ->
                editIsomorphicPane state

            EditRow subControls ->
                editRowPane subControls

            EditColumn subControls ->
                editColumnPane subControls

            EditNote state ->
                editNotePane state

            EditChord state ->
                editChordPane state

            EditCCValue state ->
                editCCValuePane state

            EditFader state ->
                editFaderPane state

            EditMidiLog ->
                acceptOrCloseButtons
                    "Ok"
                    (Just <| FinishedEdit <| Controller.MidiLog)

            EditSpace ->
                acceptOrCloseButtons
                    "Ok"
                    (Just <| FinishedEdit <| Controller.Space)
        ]


editModulePane : String -> Controller -> Element Msg
editModulePane label subController =
    column
        [ alignTop
        , padding 10
        , spacing 10
        , backgroundColour White
        , Border.width 4
        ]
        [ editTextBox
            { placeholder = "label"
            , label = "Label"
            , current = label
            }
            (\newLabel ->
                EditModule newLabel subController
                    |> UpdateControllerState
            )
        , acceptOrCloseButtons
            "Ok"
            (Just <| FinishedEdit <| Controller.Module label subController)
        ]


editIsomorphicPane : EController.EditIsomorphicState -> Element Msg
editIsomorphicPane state =
    column
        [ alignTop
        , padding 10
        , spacing 10
        , backgroundColour White
        , Border.width 4
        ]
        [ editTextBox
            { placeholder = "channel#"
            , label = "Channel"
            , current = state.channel
            }
            (\newChannel ->
                { state | channel = newChannel }
                    |> EditIsomorphic
                    |> UpdateControllerState
            )
        , editTextBox
            { placeholder = "velocity"
            , label = "Velocity"
            , current = state.velocity
            }
            (\newVelocity ->
                { state | velocity = newVelocity }
                    |> EditIsomorphic
                    |> UpdateControllerState
            )
        , editTextBox
            { placeholder = "first note#"
            , label = "First Note"
            , current = state.firstNote
            }
            (\newFirstNote ->
                { state | firstNote = newFirstNote }
                    |> EditIsomorphic
                    |> UpdateControllerState
            )
        , editTextBox
            { placeholder = "num rows"
            , label = "Number of Rows"
            , current = state.numberOfRows
            }
            (\newNumRows ->
                { state | numberOfRows = newNumRows }
                    |> EditIsomorphic
                    |> UpdateControllerState
            )
        , editTextBox
            { placeholder = "offset"
            , label = "Note Offset"
            , current = state.offset
            }
            (\newOffset ->
                { state | offset = newOffset }
                    |> EditIsomorphic
                    |> UpdateControllerState
            )
        , editTextBox
            { placeholder = "row length"
            , label = "Row Length"
            , current = state.rowLength
            }
            (\newRowLength ->
                { state | rowLength = newRowLength }
                    |> EditIsomorphic
                    |> UpdateControllerState
            )
        , acceptOrCloseButtons
            "Ok"
            (Maybe.map
                (\i -> FinishedEdit (isomorphicKeyboard i))
                (EController.toIsomorphicInput state)
            )
        ]


editRowPane : List Controller -> Element Msg
editRowPane subControls =
    column
        [ alignTop
        , padding 10
        , spacing 10
        , backgroundColour White
        , Border.width 4
        ]
        [ row [ spacing 10 ]
            [ Input.button
                [ padding 5
                , Border.width 2
                , Border.solid
                , borderColour Black
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
                , borderColour Black
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
        , column
            [ height (px 400)
            , width fill
            , padding 2
            , spacing 4
            , scrollbarY
            , Border.width 2
            , Border.dashed
            ]
            (List.map Controller.controllerToString subControls
                |> List.map text
            )
        , acceptOrCloseButtons
            "Ok"
            (Just <| FinishedEdit <| Controller.Row subControls)
        ]


editColumnPane : List Controller -> Element Msg
editColumnPane subControls =
    column
        [ alignTop
        , padding 10
        , spacing 10
        , backgroundColour White
        , Border.width 4
        ]
        [ row [ spacing 10 ]
            [ Input.button
                [ padding 5
                , Border.width 2
                , Border.solid
                , borderColour Black
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
                , borderColour Black
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
        , column
            [ height (px 400)
            , width fill
            , padding 2
            , spacing 4
            , scrollbarY
            , Border.width 2
            , Border.dashed
            ]
            (List.map Controller.controllerToString subControls
                |> List.map text
            )
        , acceptOrCloseButtons
            "Ok"
            (Just <| FinishedEdit <| Controller.Column subControls)
        ]


editNotePane : EController.EditNoteState -> Element Msg
editNotePane state =
    column
        [ alignTop
        , padding 10
        , spacing 10
        , backgroundColour White
        , Border.width 4
        ]
        [ editTextBox
            { placeholder = "label"
            , label = "Label"
            , current = state.label
            }
            (\newLabel ->
                { state | label = newLabel }
                    |> EditNote
                    |> UpdateControllerState
            )
        , colourRadio
            state.colour
            (\newColour ->
                { state | colour = newColour }
                    |> EditNote
                    |> UpdateControllerState
            )
        , editTextBox
            { placeholder = "channel#"
            , label = "Channel"
            , current = state.channel
            }
            (\newChannel ->
                { state | channel = newChannel }
                    |> EditNote
                    |> UpdateControllerState
            )
        , editTextBox
            { placeholder = "note#"
            , label = "Note Number"
            , current = state.pitch
            }
            (\newNoteNumber ->
                { state | pitch = newNoteNumber }
                    |> EditNote
                    |> UpdateControllerState
            )
        , editTextBox
            { placeholder = "velocity"
            , label = "Velocity"
            , current = state.velocity
            }
            (\newVelocity ->
                { state | velocity = newVelocity }
                    |> EditNote
                    |> UpdateControllerState
            )
        , acceptOrCloseButtons
            "Ok"
            (Maybe.map
                (\c -> FinishedEdit c)
                (EController.editStateToNote state)
            )
        ]


editChordPane : EController.EditChordState -> Element Msg
editChordPane state =
    column
        [ alignTop
        , padding 10
        , spacing 10
        , backgroundColour White
        , Border.width 4
        ]
        [ editTextBox
            { placeholder = "label"
            , label = "Label"
            , current = state.label
            }
            (\newLabel ->
                { state | label = newLabel }
                    |> EditChord
                    |> UpdateControllerState
            )
        , colourRadio
            state.colour
            (\newColour ->
                { state | colour = newColour }
                    |> EditChord
                    |> UpdateControllerState
            )
        , editTextBox
            { placeholder = "velocity"
            , label = "Velocity"
            , current = state.velocity
            }
            (\newVelocity ->
                { state | velocity = newVelocity }
                    |> EditChord
                    |> UpdateControllerState
            )
        , column
            [ padding 2
            , spacing 4
            , Border.width 2
            , Border.dashed
            ]
            [ row [ spacing 4 ]
                [ text "Notes"
                , Input.button
                    [ padding 5
                    , spacing 2
                    , Border.width 2
                    , Border.solid
                    , borderColour Black
                    ]
                    { onPress =
                        { state | notes = Dict.empty }
                            |> EditChord
                            |> UpdateControllerState
                            |> Just
                    , label = text "Clear"
                    }
                ]
            , column
                [ height (px 90)
                , padding 2
                , spacing 1
                , scrollbarY
                ]
                (List.map
                    (\{ channel, pitch } ->
                        text <|
                            "Ch: "
                                ++ Midi.channelToString channel
                                ++ ", pitch: "
                                ++ String.fromInt pitch
                    )
                    (Dict.values state.notes)
                )
            ]
        , acceptOrCloseButtons
            "Ok"
            (Maybe.map
                (\c -> FinishedEdit c)
                (EController.editStateToChord state)
            )
        ]


editCCValuePane : EController.EditCCValueState -> Element Msg
editCCValuePane state =
    column
        [ alignTop
        , padding 10
        , spacing 10
        , backgroundColour White
        , Border.width 4
        ]
        [ editTextBox
            { placeholder = "label"
            , label = "Label"
            , current = state.label
            }
            (\newLabel ->
                { state | label = newLabel }
                    |> EditCCValue
                    |> UpdateControllerState
            )
        , colourRadio
            state.colour
            (\newColour ->
                { state | colour = newColour }
                    |> EditCCValue
                    |> UpdateControllerState
            )
        , editTextBox
            { placeholder = "channel#"
            , label = "Channel"
            , current = state.channel
            }
            (\newChannel ->
                { state | channel = newChannel }
                    |> EditCCValue
                    |> UpdateControllerState
            )
        , editTextBox
            { placeholder = "controller#"
            , label = "Controller Number"
            , current = state.controller
            }
            (\newController ->
                { state | controller = newController }
                    |> EditCCValue
                    |> UpdateControllerState
            )
        , editTextBox
            { placeholder = "value"
            , label = "Value"
            , current = state.value
            }
            (\newValue ->
                { state | value = newValue }
                    |> EditCCValue
                    |> UpdateControllerState
            )
        , acceptOrCloseButtons
            "Ok"
            (Maybe.map
                (\c -> FinishedEdit c)
                (EController.editStateToCCValue state)
            )
        ]


editFaderPane : EController.EditFaderState -> Element Msg
editFaderPane state =
    column
        [ alignTop
        , padding 10
        , spacing 10
        , backgroundColour White
        , Border.width 4
        ]
        [ editTextBox
            { placeholder = "label"
            , label = "Label"
            , current = state.label
            }
            (\newLabel ->
                { state | label = newLabel }
                    |> EditFader
                    |> UpdateControllerState
            )
        , colourRadio
            state.colour
            (\newColour ->
                { state | colour = newColour }
                    |> EditFader
                    |> UpdateControllerState
            )
        , editTextBox
            { placeholder = "channel#"
            , label = "Channel"
            , current = state.channel
            }
            (\newChannel ->
                { state | channel = newChannel }
                    |> EditFader
                    |> UpdateControllerState
            )
        , editTextBox
            { placeholder = "cc#"
            , label = "CC Number"
            , current = state.ccNumber
            }
            (\newCCNumber ->
                { state | ccNumber = newCCNumber }
                    |> EditFader
                    |> UpdateControllerState
            )
        , editTextBox
            { placeholder = "min value"
            , label = "Min Value"
            , current = state.valueMin
            }
            (\newMinValue ->
                { state | valueMin = newMinValue }
                    |> EditFader
                    |> UpdateControllerState
            )
        , editTextBox
            { placeholder = "max value"
            , label = "Max Value"
            , current = state.valueMax
            }
            (\newMaxValue ->
                { state | valueMax = newMaxValue }
                    |> EditFader
                    |> UpdateControllerState
            )
        , acceptOrCloseButtons
            "Ok"
            (Maybe.map
                (\c -> FinishedEdit c)
                (EController.editStateToFader state)
            )
        ]


editTextBox :
    { placeholder : String
    , label : String
    , current : String
    }
    -> (String -> Msg)
    -> Element Msg
editTextBox { placeholder, label, current } msg =
    Input.text
        [ width fill
        , Border.width 2
        , Border.rounded 0
        , borderColour Black
        ]
        { onChange = msg
        , text = current
        , placeholder = Just <| Input.placeholder [] (text placeholder)
        , label = Input.labelAbove [] (text label)
        }


colourRadio : AppColour -> (AppColour -> Msg) -> Element Msg
colourRadio colour msg =
    Input.radio
        [ padding 2
        , spacing 10
        , height (px 100)
        , width fill
        , scrollbarY
        , Border.width 2
        , Border.dashed
        ]
        { onChange = msg
        , selected = Just colour
        , label =
            Input.labelAbove
                [ paddingEach { top = 0, bottom = 10, left = 0, right = 0 }
                ]
                (text "Colour")
        , options =
            [ Input.option Green (text "Green")
            , Input.option Blue (text "Blue")
            , Input.option Yellow (text "Yellow")
            , Input.option Red (text "Red")
            , Input.option White (text "White")
            , Input.option LightGrey (text "Light Grey")
            , Input.option DarkGrey (text "Dark Grey")
            , Input.option Black (text "Black")
            ]
        }


acceptOrCloseButtons : String -> Maybe Msg -> Element Msg
acceptOrCloseButtons acceptString acceptMsg =
    row
        [ alignTop
        , spacing 4
        , backgroundColour White
        ]
        [ Input.button
            [ padding 5
            , Border.width 2
            , Border.solid
            , borderColour Black
            ]
            { onPress = acceptMsg
            , label = text acceptString
            }
        , Input.button
            [ padding 5
            , Border.width 2
            , Border.solid
            , borderColour Black
            ]
            { onPress = Just ClosePopUp, label = text "Cancel" }
        ]



-- }}}
-- {{{ New/Edit Page Menu


newPageMenu : PageMenuState -> Element Msg
newPageMenu state =
    el [ centerX, centerY ] <|
        column
            [ padding 10
            , spacing 10
            , backgroundColour White
            , Border.width 4
            ]
            [ paragraph [ Font.bold ] [ text "New Page" ]
            , Input.text
                [ Border.width 2
                , Border.rounded 0
                , borderColour Black
                ]
                { onChange =
                    \newLabel ->
                        { state | label = newLabel }
                            |> UpdatePageMenuState
                , text = state.label
                , placeholder = Just <| Input.placeholder [] (text "page label")
                , label = Input.labelAbove [] (text "Page Label")
                }
            , row [ spacing 2 ]
                [ Input.button
                    [ padding 5
                    , Border.width 2
                    , Border.solid
                    , borderColour Black
                    ]
                    { onPress = Just (AddPage state), label = text "Add Page" }
                , Input.button
                    [ padding 5
                    , Border.width 2
                    , Border.solid
                    , borderColour Black
                    ]
                    { onPress = Just ClosePopUp, label = text "Cancel" }
                ]
            ]


editPageMenu : Int -> PageMenuState -> Element Msg
editPageMenu index state =
    el [ centerX, centerY ] <|
        column
            [ padding 10
            , spacing 10
            , backgroundColour White
            , Border.width 4
            ]
            [ paragraph [ Font.bold ] [ text "Edit Page" ]
            , Input.button
                [ padding 5
                , Border.width 2
                , Border.solid
                , backgroundColour Red
                , fontColour White
                , borderColour Black
                ]
                { onPress = Just (DeletePage index), label = text "Delete" }
            , Input.text
                [ Border.width 2
                , Border.rounded 0
                , borderColour Black
                ]
                { onChange =
                    \newLabel ->
                        { state | label = newLabel }
                            |> UpdatePageMenuState
                , text = state.label
                , placeholder = Just <| Input.placeholder [] (text "page label")
                , label = Input.labelAbove [] (text "Page Label")
                }
            , row [ spacing 2 ]
                [ Input.button
                    [ padding 5
                    , Border.width 2
                    , Border.solid
                    , borderColour Black
                    ]
                    { onPress = Just (UpdatePage index state), label = text "Update" }
                , Input.button
                    [ padding 5
                    , Border.width 2
                    , Border.solid
                    , borderColour Black
                    ]
                    { onPress = Just ClosePopUp, label = text "Cancel" }
                ]
            ]



-- }}}
-- }}}
-- {{{ Render Page


renderPage : Mode -> List String -> Page -> Element Msg
renderPage mode midiLog page =
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
        renderController mode midiLog config [] controller 0


renderController :
    Mode
    -> List String
    -> PageConfig
    -> List String
    -> Controller
    -> Int
    -> Element Msg
renderController mode midiLog config idParts controller id =
    let
        updatedParts =
            String.fromInt id :: idParts
    in
    case controller of
        Controller.Module label subControls ->
            Lazy.lazy2
                column
                ([ padding config.gapSize
                 , spacing config.gapSize
                 ]
                    ++ fillSpace
                    ++ (case mode of
                            Normal ->
                                [ Border.dotted
                                , Border.width 4
                                ]

                            Edit _ ->
                                [ padding 2
                                , Border.width 2
                                , Border.dashed
                                ]
                       )
                )
                [ row [ width fill ]
                    ((el [ padding 4 ] <| text label)
                        :: (case mode of
                                Normal ->
                                    []

                                Edit _ ->
                                    [ el
                                        [ alignRight
                                        , padding 4
                                        , backgroundColour White
                                        ]
                                      <|
                                        renderEditButton config
                                            Controller.EditContainer
                                            (updatedParts
                                                |> List.reverse
                                                |> String.join "_"
                                            )
                                    ]
                           )
                    )
                , renderController mode midiLog config updatedParts subControls 0
                ]

        Controller.Row subControls ->
            Lazy.lazy2 row
                (spacingXY config.gapSize 0
                    :: fillSpace
                    ++ (case mode of
                            Normal ->
                                []

                            Edit _ ->
                                [ padding 2
                                , Border.width 2
                                , Border.dashed
                                ]
                       )
                )
            <|
                (List.map2
                    (renderController mode midiLog config updatedParts)
                    subControls
                    (List.range 0 <| List.length subControls)
                    ++ (case mode of
                            Normal ->
                                []

                            Edit _ ->
                                [ el
                                    [ alignRight
                                    , padding 4
                                    , backgroundColour White
                                    ]
                                  <|
                                    renderEditButton config
                                        Controller.EditContainer
                                        (updatedParts
                                            |> List.reverse
                                            |> String.join "_"
                                        )
                                ]
                       )
                )

        Controller.Column subControls ->
            case mode of
                Normal ->
                    Lazy.lazy2 column
                        (spacingXY 0 config.gapSize
                            :: fillSpace
                            ++ (case mode of
                                    Normal ->
                                        []

                                    Edit _ ->
                                        [ padding 2
                                        , Border.width 2
                                        , Border.dashed
                                        ]
                               )
                        )
                    <|
                        List.map2
                            (renderController mode midiLog config updatedParts)
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
                        , column
                            ([ spacingXY 0 config.gapSize
                             , padding config.gapSize
                             ]
                                ++ fillSpace
                            )
                          <|
                            List.map2
                                (renderController mode midiLog config updatedParts)
                                subControls
                                (List.range 0 <| List.length subControls)
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

        Controller.Chord state ->
            renderChord
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

        Controller.MidiLog ->
            case mode of
                Normal ->
                    column fillSpace
                        [ column
                            ([ padding 4
                             , spacing 10
                             , backgroundColour White
                             , scrollbarY
                             , clipX
                             ]
                                ++ fillSpace
                            )
                            (if List.isEmpty midiLog then
                                [ el [ centerX, centerY ] <|
                                    text "No MIDI events in log."
                                ]

                             else
                                List.map
                                    (\s ->
                                        paragraph
                                            [ Font.alignLeft
                                            , Font.size 12
                                            , padding 2
                                            , Border.widthEach
                                                { bottom = 1, top = 0, left = 0, right = 0 }
                                            ]
                                            [ text s ]
                                    )
                                    midiLog
                            )
                        ]

                Edit _ ->
                    el
                        ([ backgroundColour LightGrey
                         , Events.onClick <|
                            OpenEditController
                                (updatedParts
                                    |> List.reverse
                                    |> String.join "_"
                                )
                         ]
                            ++ fillSpace
                        )
                        (el
                            [ centerX
                            , centerY
                            ]
                         <|
                            text "MIDI Log"
                        )

        Controller.Space ->
            case mode of
                Normal ->
                    el
                        ([ backgroundColour White
                         , borderColour LightGrey
                         , Border.width 4
                         ]
                            ++ fillSpace
                        )
                        none

                Edit _ ->
                    el
                        ([ backgroundColour LightGrey
                         , Events.onClick <|
                            OpenEditController
                                (updatedParts
                                    |> List.reverse
                                    |> String.join "_"
                                )
                         ]
                            ++ fillSpace
                        )
                        (el
                            [ centerX
                            , centerY
                            ]
                         <|
                            text "SPACE"
                        )


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
                        backgroundColour state.colour

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
                    ++ Style.noSelect
                    ++ fillSpace
                )
                ((if config.debug then
                    Midi.channelToString state.channel ++ "\n"

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
                 , backgroundColour state.colour
                 ]
                    ++ Style.noSelect
                    ++ fillSpace
                )
                { onPress = Just <| OpenEditController id
                , label =
                    state.label
                        |> text
                }


renderChord : PageConfig -> Mode -> Controller.ChordState -> String -> Element Msg
renderChord config mode state id =
    case mode of
        Normal ->
            el
                ([ padding 0
                 , spacing 0
                 , Border.width 4
                 , case state.status of
                    Controller.Off ->
                        backgroundColour state.colour

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
                    ++ Style.noSelect
                    ++ fillSpace
                )
                (state.label
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
                 , backgroundColour state.colour
                 ]
                    ++ Style.noSelect
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
                        backgroundColour state.colour

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
                    ++ Style.noSelect
                    ++ fillSpace
                )
                ((if config.debug then
                    case state.status of
                        Controller.Off ->
                            "Off\n" ++ Midi.channelToString state.channel ++ "\n"

                        Controller.On ->
                            "Off\n" ++ Midi.channelToString state.channel ++ "\n"

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
                 , backgroundColour state.colour
                 ]
                    ++ Style.noSelect
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
                        Border.solid

                    Controller.Changing _ _ ->
                        Border.dashed
                 , htmlAttribute <|
                    Touch.onStart
                        (\event ->
                            FaderChanging id event
                        )
                 , htmlAttribute <|
                    Mouse.onDown
                        (\event ->
                            FaderChangingMouse id event
                        )
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
                    ++ (case state.status of
                            Changing _ _ ->
                                [ htmlAttribute <|
                                    Mouse.onMove
                                        (\event ->
                                            FaderChangingMouse id event
                                        )
                                ]

                            Set ->
                                []
                       )
                    ++ Style.noSelect
                    ++ fillSpace
                )
                (column
                    (backgroundColour White :: fillSpace)
                    [ el [ centerX, padding 10, Font.size 16 ] <|
                        text (String.fromInt state.valuePercent ++ "%")
                    , column
                        fillSpace
                        [ el
                            [ height <| fillPortion (100 - state.valuePercent)
                            , width fill
                            , backgroundColour White
                            ]
                            none
                        , el
                            [ height <| fillPortion state.valuePercent
                            , width fill
                            , backgroundColour state.colour
                            , Border.widthEach { bottom = 0, top = 8, left = 0, right = 0 }
                            ]
                            none
                        ]
                    , el [ centerX, padding 10, Font.size 16 ] <| text state.label
                    ]
                )

        Edit _ ->
            Input.button
                ([ padding config.gapSize
                 , spacing config.gapSize
                 , Border.width 2
                 , Border.dashed
                 , Font.size 14
                 , backgroundColour state.colour
                 ]
                    ++ Style.noSelect
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
                        |> Icons.withSize 20
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
                        |> Icons.withSize 20
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
                        |> Icons.withSize 20
                        |> Icons.toHtml []
                        |> html
                }



-- }}}
-- }}}
-- {{{ PROGRAM


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
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
