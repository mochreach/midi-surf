module Main exposing (..)

import Array exposing (Array)
import Base64
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Navigation
import Bytes
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Codec exposing (Codec, Value)
import Controller as C exposing (Controller(..), FaderStatus(..), controllerCodec, setChannel)
import Dict exposing (Dict)
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
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Flate
import Html
import Html.Attributes as HAtt
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Json.Decode as Jde
import Midi exposing (EditMidiButtonMsg(..), MidiMsg(..), Status(..))
import Ports
import Style exposing (..)
import Task
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as UParser exposing ((<?>))
import Url.Parser.Query as QParser
import Utils


version : String
version =
    "0.2.0"


date : String
date =
    "2023-01-22"



-- {{{ MODEL


type alias Model =
    { midiStatus : Status
    , mode : Mode
    , pages : Array Page
    , activePage : Int
    , savedPages : Dict String Page
    , savedModules : Dict String Controller
    , menuOpen : Bool
    , popup : Maybe PopUp
    , midiLog : List String
    , screen : Maybe Screen
    }


modelCodec : Codec Model
modelCodec =
    Codec.object Model
        |> Codec.field "midiStatus" .midiStatus (Codec.constant Midi.Initialising)
        |> Codec.field "mode" .mode (Codec.constant Normal)
        |> Codec.field "pages" .pages (Codec.array pageCodec)
        |> Codec.field "activePage" .activePage (Codec.constant 0)
        |> Codec.field "savedPages" .savedPages (Codec.dict pageCodec)
        |> Codec.field "savedModules" .savedModules (Codec.dict controllerCodec)
        |> Codec.field "menuOpen" .menuOpen (Codec.constant True)
        |> Codec.field "popup" .popup (Codec.constant Nothing)
        |> Codec.field "midiLog" .midiLog (Codec.constant [])
        |> Codec.maybeField "screen" .screen screenCodec
        |> Codec.buildObject


type alias Screen =
    { width : Int
    , height : Int
    }


screenCodec : Codec Screen
screenCodec =
    Codec.object Screen
        |> Codec.field "width" .width Codec.int
        |> Codec.field "height" .height Codec.int
        |> Codec.buildObject


type Mode
    = Normal
    | Edit Bool


resetMode : Mode -> Mode
resetMode mode =
    case mode of
        Normal ->
            Normal

        Edit False ->
            Normal

        Edit True ->
            Edit True


type PopUp
    = InfoPanel
    | MidiMenu
    | SaveMenu SaveMenuState
    | ShareMenu (Maybe Page)
    | EditMenu String EditableController
    | NewPageMenu PageMenuState
    | ImportPageFromUrl String
    | EditPageMenu Int PageMenuState


type alias PageMenuState =
    { label : String
    , mode : PageMenuMode
    , mSelectedPage : Maybe Int
    , mImportedPage : Maybe Page
    , mImportError : Maybe String
    , bulkEditChannel : Maybe String
    }


type PageMenuMode
    = NewPage
    | LoadPage
    | ImportPage


type alias SaveMenuState =
    { pages : Array Page
    , mSelectedPage : Maybe Int
    , modules : List Controller
    , mSelectedModule : Maybe Int
    , mode : SaveMode
    }


type SaveMode
    = SavePage
    | SaveModule


type alias Page =
    { label : String
    , controller : Controller
    , config : PageConfig
    }


pageCodec : Codec Page
pageCodec =
    Codec.object Page
        |> Codec.field "label" .label Codec.string
        |> Codec.field "controller" .controller C.controllerCodec
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
        |> Maybe.andThen (C.getWithId "0" id)


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
                            C.updateWithId "0" p.controller updateInfo
                    }

                else
                    p
            )


type alias Flags =
    { mInitialState : Value }


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init { mInitialState } url _ =
    let
        defaultCmds =
            [ Task.perform
                (\viewport ->
                    PageResized
                        (floor viewport.scene.width)
                        (floor viewport.scene.height)
                )
                Browser.Dom.getViewport
            ]

        mPageString =
            Maybe.map
                identity
                (UParser.parse (UParser.query <| QParser.string "page") url)
    in
    case Codec.decodeValue (Codec.maybe modelCodec) mInitialState of
        Ok (Just model) ->
            ( { model
                | popup =
                    case mPageString of
                        Just (Just pageString) ->
                            Just <| ImportPageFromUrl pageString

                        _ ->
                            Just InfoPanel
              }
            , Cmd.batch defaultCmds
            )

        _ ->
            ( { midiStatus = Midi.Initialising
              , mode = Normal
              , pages = Array.fromList <| [ drumPage, synthWidePage, blankPage ]
              , activePage = 0
              , savedPages = Dict.empty
              , savedModules = Dict.empty
              , menuOpen = True
              , popup = Just <| InfoPanel
              , midiLog = []
              , screen = Nothing
              }
            , Cmd.batch defaultCmds
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
        , debug = False
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
        |> C.Column


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
                C.newNote "" Small (Style.pitchToAppColour i) channel i velocity
            )
        |> C.Row



-- {{{ Presets


synthWidePage : Page
synthWidePage =
    { label = "🎹"
    , controller = synthWideController
    , config =
        { gapSize = 2
        , debug = False
        }
    }


synthWideController : Controller
synthWideController =
    C.Row
        [ C.Column
            [ C.Row
                [ C.XYFader
                    { status = C.Set
                    , label = "P1"
                    , labelSize = Just Small
                    , colour = White
                    , channel1 = Midi.Ch6
                    , ccNumber1 = 1
                    , valuePercent1 = 50
                    , valueMin1 = 0
                    , valueMax1 = 0
                    , channel2 = Midi.Ch6
                    , ccNumber2 = 2
                    , valuePercent2 = 50
                    , valueMin2 = 0
                    , valueMax2 = 0
                    }
                , C.XYFader
                    { status = C.Set
                    , label = "Fil"
                    , labelSize = Just Small
                    , colour = White
                    , channel1 = Midi.Ch6
                    , ccNumber1 = 3
                    , valuePercent1 = 50
                    , valueMin1 = 0
                    , valueMax1 = 0
                    , channel2 = Midi.Ch6
                    , ccNumber2 = 4
                    , valuePercent2 = 50
                    , valueMin2 = 0
                    , valueMax2 = 0
                    }
                , C.PitchBend
                    { status = C.Set
                    , label = "Ch6 Bend"
                    , labelSize = Just Small
                    , colour = DarkGrey
                    , channel = Midi.Ch6
                    , bendValue = 8192
                    }
                ]
            , C.Row
                [ C.Fader
                    { status = C.Set
                    , label = "Att"
                    , labelSize = Just Small
                    , colour = Green
                    , channel = Midi.Ch6
                    , ccNumber = 5
                    , valuePercent = 50
                    , valueMin = 0
                    , valueMax = 0
                    }
                , C.Fader
                    { status = C.Set
                    , label = "Dec"
                    , labelSize = Just Small
                    , colour = Blue
                    , channel = Midi.Ch6
                    , ccNumber = 6
                    , valuePercent = 50
                    , valueMin = 0
                    , valueMax = 0
                    }
                , C.Fader
                    { status = C.Set
                    , label = "Sus"
                    , labelSize = Just Small
                    , colour = Yellow
                    , channel = Midi.Ch6
                    , ccNumber = 7
                    , valuePercent = 50
                    , valueMin = 0
                    , valueMax = 0
                    }
                , C.Fader
                    { status = C.Set
                    , label = "Rel"
                    , labelSize = Just Small
                    , colour = Red
                    , channel = Midi.Ch6
                    , ccNumber = 8
                    , valuePercent = 50
                    , valueMin = 0
                    , valueMax = 0
                    }
                ]
            ]
        , C.Module "Isomorphic Keyboard (Ch 6)" <|
            isomorphicKeyboard
                { channel = Midi.Ch6
                , velocity = 100
                , firstNote = 48
                , numberOfRows = 8
                , offset = 5
                , rowLength = 9
                }
        ]


drumPage : Page
drumPage =
    { label = "🥁"
    , controller = drumsController
    , config =
        { gapSize = 2
        , debug = False
        }
    }


drumsController : Controller
drumsController =
    C.Column
        [ C.Column
            [ C.Row
                [ C.CCValue
                    { status = C.Off
                    , label = "Unmute\nKick"
                    , labelSize = Just Small
                    , colour = LightGrey
                    , channel = Midi.Ch1
                    , controller = 53
                    , value = 0
                    }
                , C.CCValue
                    { status = C.Off
                    , label = "Unmute\nSnare"
                    , labelSize = Just Small
                    , colour = LightGrey
                    , channel = Midi.Ch2
                    , controller = 53
                    , value = 0
                    }
                , C.CCValue
                    { status = C.Off
                    , label = "Unmute\nHat"
                    , labelSize = Just Small
                    , colour = LightGrey
                    , channel = Midi.Ch3
                    , controller = 53
                    , value = 0
                    }
                ]
            , C.Row
                [ C.CCValue
                    { status = C.Off
                    , label = "Mute\nKick"
                    , labelSize = Just Small
                    , colour = DarkGrey
                    , channel = Midi.Ch1
                    , controller = 53
                    , value = 1
                    }
                , C.CCValue
                    { status = C.Off
                    , label = "Mute\nSnare"
                    , labelSize = Just Small
                    , colour = DarkGrey
                    , channel = Midi.Ch2
                    , controller = 53
                    , value = 1
                    }
                , C.CCValue
                    { status = C.Off
                    , label = "Mute\nHat"
                    , labelSize = Just Small
                    , colour = DarkGrey
                    , channel = Midi.Ch3
                    , controller = 53
                    , value = 1
                    }
                ]
            , C.Row
                [ C.Command
                    { status = C.Off
                    , label = "Hold for\nReverse Snare"
                    , labelSize = Just Small
                    , colour = Yellow
                    , onPressMsgs =
                        [ Midi.ControllerChange
                            { channel = 1
                            , controller = 2
                            , value = 127
                            }
                        ]
                    , onReleaseMsgs =
                        [ Midi.ControllerChange
                            { channel = 1
                            , controller = 2
                            , value = 0
                            }
                        ]
                    }
                , C.Sequence
                    { status = C.Off
                    , label = "Play/Stop"
                    , labelSize = Just Small
                    , colour = White
                    , midiMsgs =
                        Array.fromList
                            [ Midi.StartSong
                            , Midi.StopSong
                            ]
                    , index = 0
                    }
                ]
            ]
        , Row
            [ C.Note
                { status = C.Off
                , label = "Snare"
                , labelSize = Just Small
                , colour = Green
                , channel = Midi.Ch2
                , pitch = 53
                , velocity = 100
                }
            , C.Note
                { status = C.Off
                , label = "Hat 50"
                , labelSize = Just Small
                , colour = Blue
                , channel = Midi.Ch3
                , pitch = 53
                , velocity = 50
                }
            ]
        , Row
            [ C.Note
                { status = C.Off
                , label = "Kick"
                , labelSize = Just Small
                , colour = Red
                , channel = Midi.Ch1
                , pitch = 53
                , velocity = 100
                }
            , C.Note
                { status = C.Off
                , label = "Hat 100"
                , labelSize = Just Small
                , colour = Blue
                , channel = Midi.Ch3
                , pitch = 53
                , velocity = 100
                }
            ]
        ]


blankPage : Page
blankPage =
    { label = "Blank"
    , controller = C.Module "Click the pencil in\n the menu to edit me!" C.Space
    , config =
        { gapSize = 2
        , debug = False
        }
    }



-- }}}
-- }}}
-- {{{ UPDATE


type Msg
    = MidiDevicesChanged (List Midi.Device)
    | ToggleMenu
    | OpenInfoPanel
    | OpenMidiMenu
    | OpenSaveLoadMenu
    | UpdateSaveMenuState SaveMenuState
    | SaveSelectedPage Page
    | SaveSelectedModule Controller
    | ExportSelectedPage Page
    | OpenShareMenu
    | ToggleNormalEdit
    | OpenEditPageMenu Int
    | DeletePage Int
    | OpenNewPageMenu
    | UpdatePageMenuState PageMenuState
    | DeleteSavedPage String
    | ImportPageRequested
    | ReceivedPage File
    | PageImported String
    | AddPage Page
    | UpdatePage Int PageMenuState
    | AddSpace String
    | RemoveItem String
    | OpenEditController String
    | SetEditType EditableController
    | UpdateControllerState EditableController
    | DeleteSavedModule String
    | FinishedEdit Controller
    | SelectActivePage Int
    | ButtonDown String
    | ButtonUp String
    | FaderChanging String Touch.Event
    | FaderChangingMouse String Mouse.Event
    | FaderSet String
    | IncomingMidi { deviceName : String, midiData : Array Int }
    | PageResized Int Int
    | CopyToClipboard String
    | ClosePopUp
    | NoOp


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
                            { pages = model.pages
                            , mSelectedPage = Nothing
                            , modules =
                                Array.map .controller model.pages
                                    |> Array.toList
                                    |> List.map (C.getModules [])
                                    |> List.concat
                            , mSelectedModule = Nothing
                            , mode = SavePage
                            }
              }
            , Cmd.none
            )

        UpdateSaveMenuState state ->
            case model.popup of
                Just (SaveMenu _) ->
                    ( { model
                        | popup =
                            Just <|
                                SaveMenu state
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model
                        | popup = Nothing
                      }
                    , Cmd.none
                    )

        SaveSelectedPage page ->
            let
                ( key, updatedPage ) =
                    if Dict.member page.label model.savedPages then
                        let
                            newLabel =
                                page.label ++ "_0"
                        in
                        ( newLabel, { page | label = newLabel } )

                    else
                        ( page.label, page )

                newModel =
                    { model
                        | savedPages = Dict.insert key updatedPage model.savedPages
                        , popup = Nothing
                    }
            in
            ( newModel
            , Ports.saveState <|
                Codec.encodeToValue modelCodec newModel
            )

        SaveSelectedModule controller ->
            case controller of
                (C.Module label subControl) as modu ->
                    let
                        ( key, updatedModule ) =
                            if Dict.member label model.savedModules then
                                let
                                    newLabel =
                                        label ++ "_0"
                                in
                                ( newLabel, C.Module newLabel subControl )

                            else
                                ( label, modu )

                        newModel =
                            { model
                                | savedModules =
                                    Dict.insert key updatedModule model.savedModules
                                , popup = Nothing
                            }
                    in
                    ( newModel
                    , Ports.saveState <|
                        Codec.encodeToValue modelCodec newModel
                    )

                _ ->
                    ( model, Cmd.none )

        ExportSelectedPage page ->
            ( { model | popup = Nothing }
            , Download.string
                "midisurf.json"
                "text/json"
                (Codec.encodeToString 0 pageCodec page)
            )

        OpenShareMenu ->
            ( { model
                | popup =
                    Array.get model.activePage model.pages
                        |> ShareMenu
                        |> Just
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
                                EditPageMenu index
                                    { label = page.label
                                    , mode = NewPage
                                    , mSelectedPage = Nothing
                                    , mImportedPage = Nothing
                                    , mImportError = Nothing
                                    , bulkEditChannel = Nothing
                                    }
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
                        , mode = resetMode model.mode
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
                        NewPageMenu
                            { label = ""
                            , mode = NewPage
                            , mSelectedPage = Nothing
                            , mImportedPage = Nothing
                            , mImportError = Nothing
                            , bulkEditChannel = Nothing
                            }
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
                                , mode = resetMode model.mode
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

        DeleteSavedPage key ->
            ( { model | savedPages = Dict.remove key model.savedPages }
            , Cmd.none
            )

        ImportPageRequested ->
            ( model
            , Select.file [ "text/json" ] ReceivedPage
            )

        ReceivedPage file ->
            ( model
            , Task.perform PageImported (File.toString file)
            )

        PageImported string ->
            ( case model.popup of
                Just (NewPageMenu state) ->
                    { model
                        | popup =
                            Just <|
                                NewPageMenu
                                    (case Codec.decodeString pageCodec string of
                                        Ok page ->
                                            { state
                                                | mImportedPage = Just page
                                                , mImportError = Nothing
                                            }

                                        Err error ->
                                            { state
                                                | mImportedPage = Nothing
                                                , mImportError = Just <| Jde.errorToString error
                                            }
                                    )
                    }

                _ ->
                    model
            , Cmd.none
            )

        AddPage page ->
            let
                newModel =
                    { model
                        | pages =
                            Array.push page model.pages

                        -- I don't need to subtract 1 for zero indexing
                        -- as this is the old array length
                        , activePage = Array.length model.pages
                        , popup = Nothing
                        , mode = resetMode model.mode
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
                        { id = id, updateFn = C.addSpace }
                        model.pages
              }
            , Cmd.none
            )

        RemoveItem id ->
            ( { model
                | pages =
                    updateControllerOnActivePage
                        model.activePage
                        { id = id, updateFn = C.removeItem }
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
                    Maybe.map (EditMenu id << convertToEditable) control
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
                                , mode = resetMode model.mode
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

        DeleteSavedModule key ->
            ( { model
                | savedModules =
                    Dict.remove key model.savedModules
              }
            , Cmd.none
            )

        ButtonDown id ->
            case getControllerFromActivePage id model.activePage model.pages of
                Just (C.Module _ _) ->
                    ( model, Cmd.none )

                Just (C.Column _) ->
                    ( model, Cmd.none )

                Just (C.Row _) ->
                    ( model, Cmd.none )

                Just ((C.Note _) as note) ->
                    let
                        ( updatedNote, midiMsgs ) =
                            C.buttonOn note
                    in
                    ( { model
                        | pages =
                            updateControllerOnActivePage
                                model.activePage
                                { id = id, updateFn = always updatedNote }
                                model.pages
                      }
                    , List.map Midi.midiMsgToIntArray midiMsgs
                        |> Array.fromList
                        |> Ports.outgoingMidi
                    )

                Just ((C.Chord _) as chord) ->
                    let
                        ( updatedChord, midiMsgs ) =
                            C.buttonOn chord
                    in
                    ( { model
                        | pages =
                            updateControllerOnActivePage
                                model.activePage
                                { id = id, updateFn = always updatedChord }
                                model.pages
                      }
                    , List.map Midi.midiMsgToIntArray midiMsgs
                        |> Array.fromList
                        |> Ports.outgoingMidi
                    )

                Just ((C.CCValue _) as ccValue) ->
                    let
                        ( updatedCCValue, midiMsgs ) =
                            C.buttonOn ccValue
                    in
                    ( { model
                        | pages =
                            updateControllerOnActivePage
                                model.activePage
                                { id = id, updateFn = always updatedCCValue }
                                model.pages
                      }
                    , List.map Midi.midiMsgToIntArray midiMsgs
                        |> Array.fromList
                        |> Ports.outgoingMidi
                    )

                Just ((C.Command _) as command) ->
                    let
                        ( updatedCommand, midiMsgs ) =
                            C.buttonOn command
                    in
                    ( { model
                        | pages =
                            updateControllerOnActivePage
                                model.activePage
                                { id = id, updateFn = always updatedCommand }
                                model.pages
                      }
                    , List.map Midi.midiMsgToIntArray midiMsgs
                        |> Array.fromList
                        |> Ports.outgoingMidi
                    )

                Just ((C.Sequence _) as sequence) ->
                    let
                        ( updatedSequence, midiMsgs ) =
                            C.buttonOn sequence
                    in
                    ( { model
                        | pages =
                            updateControllerOnActivePage
                                model.activePage
                                { id = id, updateFn = always updatedSequence }
                                model.pages
                      }
                    , List.map Midi.midiMsgToIntArray midiMsgs
                        |> Array.fromList
                        |> Ports.outgoingMidi
                    )

                Just (C.Fader _) ->
                    ( model, Cmd.none )

                Just (C.XYFader _) ->
                    ( model, Cmd.none )

                Just (C.PitchBend _) ->
                    ( model, Cmd.none )

                Just C.MidiLog ->
                    ( model, Cmd.none )

                Just C.Space ->
                    ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ButtonUp id ->
            case getControllerFromActivePage id model.activePage model.pages of
                Just (C.Module _ _) ->
                    ( model, Cmd.none )

                Just (C.Column _) ->
                    ( model, Cmd.none )

                Just (C.Row _) ->
                    ( model, Cmd.none )

                Just ((C.Note _) as note) ->
                    let
                        ( updatedNote, midiMsgs ) =
                            C.buttonOff note
                    in
                    ( { model
                        | pages =
                            updateControllerOnActivePage
                                model.activePage
                                { id = id, updateFn = always updatedNote }
                                model.pages
                      }
                    , List.map Midi.midiMsgToIntArray midiMsgs
                        |> Array.fromList
                        |> Ports.outgoingMidi
                    )

                Just ((C.Chord _) as chord) ->
                    let
                        ( updatedChord, midiMsgs ) =
                            C.buttonOff chord
                    in
                    ( { model
                        | pages =
                            updateControllerOnActivePage
                                model.activePage
                                { id = id, updateFn = always updatedChord }
                                model.pages
                      }
                    , List.map Midi.midiMsgToIntArray midiMsgs
                        |> Array.fromList
                        |> Ports.outgoingMidi
                    )

                Just ((C.CCValue _) as ccValue) ->
                    let
                        ( updatedCCValue, _ ) =
                            C.buttonOff ccValue
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

                Just ((C.Command _) as command) ->
                    let
                        ( updatedCommand, midiMsgs ) =
                            C.buttonOff command
                    in
                    ( { model
                        | pages =
                            updateControllerOnActivePage
                                model.activePage
                                { id = id, updateFn = always updatedCommand }
                                model.pages
                      }
                    , List.map Midi.midiMsgToIntArray midiMsgs
                        |> Array.fromList
                        |> Ports.outgoingMidi
                    )

                Just ((C.Sequence _) as sequence) ->
                    let
                        ( updatedSequence, midiMsgs ) =
                            C.buttonOff sequence
                    in
                    ( { model
                        | pages =
                            updateControllerOnActivePage
                                model.activePage
                                { id = id, updateFn = always updatedSequence }
                                model.pages
                      }
                    , List.map Midi.midiMsgToIntArray midiMsgs
                        |> Array.fromList
                        |> Ports.outgoingMidi
                    )

                Just (C.Fader _) ->
                    ( model, Cmd.none )

                Just (C.XYFader _) ->
                    ( model, Cmd.none )

                Just (C.PitchBend _) ->
                    ( model, Cmd.none )

                Just C.MidiLog ->
                    ( model, Cmd.none )

                Just C.Space ->
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
                            (C.Fader
                                { status = C.Set
                                , label = "ERROR"
                                , labelSize = Just Small
                                , colour = LightGrey
                                , channel = Midi.Ch1
                                , ccNumber = 1
                                , valuePercent = 50
                                , valueMin = 0
                                , valueMax = 127
                                }
                            )

                ( newFader, midiMsgs ) =
                    C.faderChanging identifier touchCoordinates fader
            in
            ( { model
                | pages =
                    updateControllerOnActivePage
                        model.activePage
                        { id = id, updateFn = always newFader }
                        model.pages
              }
            , List.map Midi.midiMsgToIntArray midiMsgs
                |> Array.fromList
                |> Ports.outgoingMidi
            )

        FaderChangingMouse id mouseEvent ->
            let
                fader =
                    getControllerFromActivePage id model.activePage model.pages
                        |> Maybe.withDefault
                            (C.Fader
                                { status = C.Set
                                , label = "ERROR"
                                , labelSize = Just Small
                                , colour = LightGrey
                                , channel = Midi.Ch1
                                , ccNumber = 1
                                , valuePercent = 50
                                , valueMin = 0
                                , valueMax = 127
                                }
                            )

                ( newFader, midiMsgs ) =
                    C.faderChanging -1 mouseEvent.clientPos fader
            in
            ( { model
                | pages =
                    updateControllerOnActivePage
                        model.activePage
                        { id = id, updateFn = always newFader }
                        model.pages
              }
            , List.map Midi.midiMsgToIntArray midiMsgs
                |> Array.fromList
                |> Ports.outgoingMidi
            )

        FaderSet id ->
            let
                fader =
                    getControllerFromActivePage id model.activePage model.pages
                        |> Maybe.withDefault
                            (C.Fader
                                { status = C.Set
                                , label = "ERROR"
                                , labelSize = Just Small
                                , colour = LightGrey
                                , channel = Midi.Ch1
                                , ccNumber = 1
                                , valuePercent = 50
                                , valueMin = 0
                                , valueMax = 127
                                }
                            )

                ( newFader, midiMsgs ) =
                    C.faderSet fader
            in
            ( { model
                | pages =
                    updateControllerOnActivePage
                        model.activePage
                        { id = id, updateFn = always newFader }
                        model.pages
              }
            , List.map Midi.midiMsgToIntArray midiMsgs
                |> Array.fromList
                |> Ports.outgoingMidi
            )

        PageResized width height ->
            ( { model
                | screen = Just { width = width, height = height }
              }
            , Cmd.none
            )

        CopyToClipboard string ->
            ( model
            , Ports.copyToClipboard string
            )

        ClosePopUp ->
            ( { model | popup = Nothing, mode = resetMode model.mode }
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

        NoOp ->
            ( model, Cmd.none )


convertToEditable : Controller -> EditableController
convertToEditable control =
    case control of
        C.Module label subController ->
            EditModule
                { label = label
                , controller = subController
                , createMode = EController.New
                , selectedModule = Nothing
                }

        C.Row subControls ->
            EditRow subControls

        C.Column subControls ->
            EditColumn subControls

        C.Note { label, labelSize, colour, pitch, channel, velocity } ->
            EditNote
                { label = label
                , labelSize = Maybe.withDefault Small labelSize
                , colour = colour
                , pitch = String.fromInt pitch
                , channel = Midi.channelToString channel
                , velocity = String.fromInt velocity
                }

        C.Chord { label, labelSize, colour, velocity, notes } ->
            EditChord
                { label = label
                , labelSize = Maybe.withDefault Small labelSize
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

        C.CCValue { label, labelSize, colour, channel, controller, value } ->
            EditCCValue
                { label = label
                , labelSize = Maybe.withDefault Small labelSize
                , colour = colour
                , channel = Midi.channelToString channel
                , controller = String.fromInt controller
                , value = String.fromInt value
                }

        C.Command { label, labelSize, colour, onPressMsgs, onReleaseMsgs } ->
            EditCommand
                { label = label
                , labelSize = Maybe.withDefault Small labelSize
                , colour = colour
                , editMode = EController.OnPressMsgs
                , onPressMsgs = onPressMsgs
                , onReleaseMsgs = onReleaseMsgs
                , newMsg = Nothing
                }

        C.Sequence { label, labelSize, colour, midiMsgs } ->
            EditSequence
                { label = label
                , labelSize = Maybe.withDefault Small labelSize
                , colour = colour
                , midiMsgs = midiMsgs
                , newMsg = Nothing
                }

        C.Fader state ->
            EditFader
                { label = state.label
                , labelSize = Maybe.withDefault Small state.labelSize
                , colour = state.colour
                , channel = Midi.channelToString state.channel
                , ccNumber = String.fromInt state.ccNumber
                , valueMin = String.fromInt state.valueMin
                , valueMax = String.fromInt state.valueMax
                }

        C.XYFader state ->
            EditXYFader
                { label = state.label
                , labelSize = Maybe.withDefault Small state.labelSize
                , colour = state.colour
                , active = EController.Params1
                , channel1 = Midi.channelToString state.channel1
                , ccNumber1 = String.fromInt state.ccNumber1
                , valueMin1 = String.fromInt state.valueMin1
                , valueMax1 = String.fromInt state.valueMax1
                , channel2 = Midi.channelToString state.channel2
                , ccNumber2 = String.fromInt state.ccNumber2
                , valueMin2 = String.fromInt state.valueMin2
                , valueMax2 = String.fromInt state.valueMax2
                }

        C.PitchBend state ->
            EditPitchBend
                { label = state.label
                , labelSize = Maybe.withDefault Small state.labelSize
                , colour = state.colour
                , channel = Midi.channelToString state.channel
                }

        C.MidiLog ->
            EditMidiLog

        C.Space ->
            EditSpace



-- }}}
-- {{{ VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "MIDI Surf"
    , body =
        List.singleton <|
            layout
                ((case model.popup of
                    Just popup ->
                        (inFront <|
                            renderPopup
                                model.screen
                                model.midiStatus
                                model.savedPages
                                model.savedModules
                                popup
                        )
                            :: (case model.screen of
                                    Just screen ->
                                        [ width <| px screen.width, height <| px screen.height ]

                                    Nothing ->
                                        fillSpace
                               )

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
                    (case model.screen of
                        Just screen ->
                            [ width <| px screen.width, height <| px screen.height ]

                        Nothing ->
                            fillSpace
                    )
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
    }



-- {{{ Title Bar


titleBar : Mode -> Bool -> Int -> Array Page -> Element Msg
titleBar mode menuOpen activePage pages =
    row
        [ height <| px 76
        , width fill
        , padding 4
        , spacing 4
        , scrollbarX
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
                        , newTabLink
                            [ padding 10
                            , Border.width 4
                            , backgroundColour White
                            ]
                            { url = "https://ko-fi.com/mochreach"
                            , label =
                                Icons.coffee
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
                            , backgroundColour White
                            ]
                            { onPress = Just OpenShareMenu
                            , label =
                                Icons.share2
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


renderPopup :
    Maybe Screen
    -> Midi.Status
    -> Dict String Page
    -> Dict String Controller
    -> PopUp
    -> Element Msg
renderPopup screen midiStatus savedPages savedModules popup =
    el
        ([ padding 5, scrollbars, Background.color (rgba 0.5 0.5 0.5 0.8) ]
            ++ (case screen of
                    Just s ->
                        [ width <| px s.width
                        , height <| px s.height
                        ]

                    Nothing ->
                        fillSpace
               )
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

            SaveMenu state ->
                saveMenu state

            ShareMenu mPage ->
                shareMenu mPage

            EditMenu _ state ->
                editMenu savedModules state

            NewPageMenu state ->
                newPageMenu savedPages state

            ImportPageFromUrl pageString ->
                importPageFromUrlMenu pageString

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
                [ spacing 20, height fill, scrollbarY, Font.size 16 ]
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
                , paragraph []
                    [ text "Ver. "
                    , text version
                    , text <|
                        (" (" ++ date ++ ", ")
                    , newTabLink
                        linkStyle
                        { url = "https://github.com/mochreach/midi-surf"
                        , label = text "source"
                        }
                    , text ")"
                    ]
                , paragraph []
                    [ text <|
                        "The \"Hands On Update\" is here, scroll down for details."
                    ]
                , Html.iframe
                    [ HAtt.height 300
                    , HAtt.width 500
                    , HAtt.src "https://www.youtube.com/embed/c5BpeMxs5ZU"
                    , HAtt.title "YouTube video player"
                    , HAtt.attribute "frameborder" "0"
                    , HAtt.attribute
                        "allow"
                        "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
                    , HAtt.attribute "allowfullscreen" ""
                    ]
                    []
                    |> html
                    |> el [ centerX ]
                    |> el [ width fill ]
                , paragraph [ Font.bold ] [ text "Supporting Development" ]
                , paragraph []
                    [ """ Please consider supporting the development of this
                    app if you enjoy using it, I'd really appreciate it if you
                    did! You can join our community on """ |> text
                    , newTabLink
                        linkStyle
                        { url = "https://patreon.com/mochreach"
                        , label = text "Patreon"
                        }
                    , """ and help guide the direction the app (and future software
                    from Mo Chreach!) takes, or buy me a coffee/beer/cup of tea/sausage roll on
                    """ |> text
                    , newTabLink
                        linkStyle
                        { url = "https://ko-fi.com/mochreach"
                        , label = text "Ko-Fi"
                        }
                    , """. Check out my channel on """ |> text
                    , newTabLink
                        linkStyle
                        { url = "https://www.youtube.com/@mochreach"
                        , label = text "YouTube"
                        }
                    , """ for more videos about MIDI Surf and other music
                      technology topics.
                      """ |> text
                    ]
                , paragraph [ Font.bold ] [ text "Version History" ]
                , paragraph []
                    [ el [ Font.italic ] <| text "The Hands On Update (2022-01-22): "
                    , text <|
                        """Adds a range of new controller types (commands, sequences,
                        XY faders, pitch bend) as well as overhauling the edit menu to
                        make it better utilise screen space and adding information about
                        the controllers. I've made an effort to enable old controllers
                        to be backwards compatible, so hopefully everything should work as
                        normal.
                        """
                    ]
                , paragraph []
                    [ el [ Font.italic ] <| text "The Initial Release (2022-01-06): "
                    , text <|
                        """Hurray, I've launched the first version of the app! We have
                        basic control types like notes, chords, CC values and faders.
                        There are some obvious ommisions, but the app is useful enough to
                        be released. Please either comment on GitHub or YouTube if you have
                        any feedback!
                        """
                    ]
                , paragraph [ Font.bold ] [ text "Analytics" ]
                , paragraph []
                    [ text <|
                        """By using this application, you agree to data on your usage being
                        collected by Google Analytics. I collect the minimum amount of data
                        possible so that I can improve the app, but if you're not keen on
                        this, feel free to block it.
                        """
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


saveMenu : SaveMenuState -> Element Msg
saveMenu ({ pages, mSelectedPage, modules, mSelectedModule, mode } as state) =
    el [ centerX, centerY ] <|
        column
            [ padding 10
            , spacing 10
            , backgroundColour White
            , Border.width 4
            ]
            [ paragraph [ Font.bold ] [ text "Save" ]
            , Input.radioRow
                [ spacing 10 ]
                { onChange =
                    \newMode ->
                        { state | mode = newMode }
                            |> UpdateSaveMenuState
                , selected = Just mode
                , label = Input.labelHidden "Save Mode"
                , options =
                    [ Input.option SavePage (text "Page")
                    , Input.option SaveModule (text "Module")
                    ]
                }
            , case mode of
                SavePage ->
                    column
                        [ spacing 5, width fill ]
                        [ column
                            [ height (px 200)
                            , width fill
                            , scrollbarY
                            , Border.width 2
                            , Border.dashed
                            ]
                            (Array.map .label pages
                                |> Array.toList
                                |> List.indexedMap
                                    (\i l -> String.fromInt i ++ ": " ++ l)
                                |> List.indexedMap
                                    (selectableOption
                                        (\newSelected ->
                                            { state | mSelectedPage = Just newSelected }
                                                |> UpdateSaveMenuState
                                        )
                                        mSelectedPage
                                    )
                            )
                        , case mSelectedPage of
                            Just index ->
                                Input.button
                                    [ padding 5
                                    , Border.width 2
                                    , Border.solid
                                    ]
                                    { onPress =
                                        Array.get index pages
                                            |> Maybe.map (\p -> ExportSelectedPage p)
                                    , label = text "Export to File"
                                    }

                            Nothing ->
                                none
                        ]

                SaveModule ->
                    column
                        [ spacing 5, width fill ]
                        [ column
                            [ height (px 200)
                            , width fill
                            , scrollbarY
                            , Border.width 2
                            , Border.dashed
                            ]
                            (List.map C.controllerToString modules
                                |> List.indexedMap
                                    (selectableOption
                                        (\newSelected ->
                                            { state | mSelectedModule = Just newSelected }
                                                |> UpdateSaveMenuState
                                        )
                                        mSelectedModule
                                    )
                            )
                        ]
            , acceptOrCloseButtons
                "Save"
                ClosePopUp
                (case ( mode, mSelectedPage, mSelectedModule ) of
                    ( SavePage, Just index, _ ) ->
                        Array.get index pages
                            |> Maybe.map (\p -> SaveSelectedPage p)

                    ( SaveModule, _, Just index ) ->
                        Array.fromList modules
                            |> Array.get index
                            |> Maybe.map (\p -> SaveSelectedModule p)

                    _ ->
                        Nothing
                )
            ]



-- }}}
-- {{{ Share Menu


shareMenu : Maybe Page -> Element Msg
shareMenu mPage =
    el [ centerX, centerY ] <|
        column
            [ padding 10
            , spacing 10
            , backgroundColour White
            , Border.width 4
            ]
            [ paragraph [ Font.bold ] [ text "Share Current Page" ]
            , case mPage of
                Just page ->
                    let
                        encodedPage =
                            Codec.encodeToString 0 pageCodec page

                        compressedPage =
                            encodedPage
                                |> Encode.string
                                |> Encode.encode
                                |> Flate.deflateGZip
                                |> Base64.fromBytes
                                |> Maybe.withDefault ""

                        decompressedPage =
                            compressedPage
                                |> Base64.toBytes
                                |> Maybe.andThen Flate.inflateGZip
                                |> Maybe.andThen
                                    (\bytes ->
                                        Decode.decode (Decode.string (Bytes.width bytes)) bytes
                                    )
                                |> Maybe.withDefault ""

                        pageUrl =
                            Builder.crossOrigin
                                "https://midisurf.app"
                                []
                                [ Builder.string "page" compressedPage ]
                    in
                    row
                        [ spacing 10 ]
                        [ el
                            [ height <| px 40
                            , width <| px 280
                            , scrollbarX
                            ]
                            (text pageUrl)
                        , Input.button
                            [ padding 4
                            , Border.width 2
                            ]
                            { onPress = Just <| CopyToClipboard pageUrl
                            , label =
                                Icons.copy
                                    |> Icons.withSize 20
                                    |> Icons.toHtml []
                                    |> html
                            }
                        ]

                Nothing ->
                    paragraph
                        [ width <| px 300 ]
                        [ text "No page selected."
                        ]
            , Input.button
                [ padding 5
                , Border.width 2
                , Border.solid
                ]
                { onPress = Just ClosePopUp
                , label = text "Close"
                }
            ]



-- }}}
-- {{{ Edit Menu


editPanelWidth : Element.Attribute msg
editPanelWidth =
    width <| minimum 320 <| maximum 540 <| fill


editMenu : Dict String Controller -> EditableController -> Element Msg
editMenu savedModules menuType =
    wrappedRow
        [ spacing 4
        , width fill
        , height fill
        ]
        [ wrappedRow
            [ alignTop
            , padding 10
            , spacing 10
            , editPanelWidth
            , backgroundColour White
            , Border.width 4
            ]
            [ Input.radio
                [ spacing 10 ]
                { onChange = SetEditType
                , selected = Just menuType
                , label = Input.labelHidden "Type Selector"
                , options =
                    [ Input.option
                        (EditModule
                            { label = ""
                            , controller = C.Space
                            , createMode = EController.New
                            , selectedModule = Nothing
                            }
                        )
                        (text "Module")
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
                            EditCommand _ ->
                                menuType

                            _ ->
                                EditCommand EController.defaultEditCommandState
                        )
                        (text "Command")
                    , Input.option
                        (case menuType of
                            EditSequence _ ->
                                menuType

                            _ ->
                                EditSequence EController.defaultEditSequenceState
                        )
                        (text "Sequence")
                    , Input.option
                        (case menuType of
                            EditFader _ ->
                                menuType

                            _ ->
                                EditFader EController.defaultEditFaderState
                        )
                        (text "Fader")
                    , Input.option
                        (case menuType of
                            EditXYFader _ ->
                                menuType

                            _ ->
                                EditXYFader EController.defaultEditXYFaderState
                        )
                        (text "XY Fader")
                    , Input.option
                        (case menuType of
                            EditPitchBend _ ->
                                menuType

                            _ ->
                                EditPitchBend EController.defaultEditPitchBendState
                        )
                        (text "Pitch Bend")
                    , Input.option EditMidiLog (text "MIDI Log")
                    , Input.option EditSpace (text "Space")
                    ]
                }
            , paragraph
                [ alignTop
                , height fill
                , padding 10
                , Border.width 2
                , Border.dashed
                , Font.size 14
                , Font.alignLeft
                ]
                [ text <| EController.description menuType ]
            ]
        , case menuType of
            EditModule state ->
                editModulePane savedModules state

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

            EditCommand state ->
                editCommandPane state

            EditSequence state ->
                editSequencePane state

            EditFader state ->
                editFaderPane state

            EditXYFader state ->
                editXYFaderPane state

            EditPitchBend state ->
                editPitchBendPane state

            EditMidiLog ->
                el
                    [ alignTop
                    , padding 10
                    , backgroundColour White
                    , Border.width 4
                    ]
                <|
                    acceptOrCloseButtons
                        "Ok"
                        ClosePopUp
                        (Just <| FinishedEdit <| C.MidiLog)

            EditSpace ->
                el
                    [ alignTop
                    , padding 10
                    , backgroundColour White
                    , Border.width 4
                    ]
                <|
                    acceptOrCloseButtons
                        "Ok"
                        ClosePopUp
                        (Just <| FinishedEdit <| C.Space)
        ]


editModulePane : Dict String Controller -> EController.EditModuleState -> Element Msg
editModulePane savedModules state =
    column
        [ alignTop
        , padding 10
        , spacing 10
        , editPanelWidth
        , backgroundColour White
        , Border.width 4
        ]
        [ Input.radioRow
            [ spacing 10 ]
            { onChange =
                \newMode ->
                    { state | createMode = newMode }
                        |> EController.EditModule
                        |> UpdateControllerState
            , selected = Just state.createMode
            , label = Input.labelHidden "Create Mode"
            , options =
                [ Input.option EController.New (text "New")
                , Input.option EController.Load (text "Load")
                ]
            }
        , case state.createMode of
            EController.New ->
                editTextBox
                    { placeholder = "label"
                    , label = "Label"
                    , current = state.label
                    }
                    "text"
                    (\newLabel ->
                        { state | label = newLabel }
                            |> EditModule
                            |> UpdateControllerState
                    )

            EController.Load ->
                column [ spacing 10, width fill ]
                    [ case state.selectedModule of
                        Just index ->
                            Input.button
                                [ padding 5
                                , Border.width 2
                                , Border.solid
                                , backgroundColour Red
                                , fontColour White
                                , borderColour Black
                                ]
                                { onPress =
                                    Dict.toList savedModules
                                        |> Array.fromList
                                        |> Array.get index
                                        |> Maybe.map (\( key, _ ) -> DeleteSavedModule key)
                                , label = text "Delete"
                                }

                        Nothing ->
                            none
                    , column
                        [ height (px 200)
                        , width fill
                        , scrollbarY
                        , Border.width 2
                        , Border.dashed
                        ]
                        (Dict.keys savedModules
                            |> List.indexedMap
                                (selectableOption
                                    (\newSelected ->
                                        { state | selectedModule = Just newSelected }
                                            |> EditModule
                                            |> UpdateControllerState
                                    )
                                    state.selectedModule
                                )
                        )
                    ]
        , acceptOrCloseButtons
            "Ok"
            ClosePopUp
            (case state.createMode of
                EController.New ->
                    Just <| FinishedEdit <| C.Module state.label state.controller

                EController.Load ->
                    state.selectedModule
                        |> Maybe.andThen
                            (\i ->
                                Dict.values savedModules
                                    |> Array.fromList
                                    |> Array.get i
                                    |> Maybe.map (\m -> FinishedEdit m)
                            )
            )
        ]


editIsomorphicPane : EController.EditIsomorphicState -> Element Msg
editIsomorphicPane state =
    column
        [ alignTop
        , padding 10
        , spacing 10
        , editPanelWidth
        , backgroundColour White
        , Border.width 4
        ]
        [ editTextBox
            { placeholder = "channel#"
            , label = "Channel"
            , current = state.channel
            }
            "number"
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
            "number"
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
            "number"
            (\newFirstNote ->
                { state | firstNote = newFirstNote }
                    |> EditIsomorphic
                    |> UpdateControllerState
            )
        , editTextBox
            { placeholder = "num of rows"
            , label = "Rows"
            , current = state.numberOfRows
            }
            "number"
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
            "number"
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
            "number"
            (\newRowLength ->
                { state | rowLength = newRowLength }
                    |> EditIsomorphic
                    |> UpdateControllerState
            )
        , acceptOrCloseButtons
            "Ok"
            ClosePopUp
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
        , editPanelWidth
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
                    List.append subControls [ C.Space ]
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
        , paragraph [ Font.size 18 ] [ text "Listening for MIDI..." ]
        , column
            [ height (px 300)
            , width fill
            , padding 2
            , spacing 4
            , scrollbarY
            , Border.width 2
            , Border.dashed
            ]
            (List.map C.controllerToString subControls
                |> List.map text
            )
        , acceptOrCloseButtons
            "Ok"
            ClosePopUp
            (Just <| FinishedEdit <| C.Row subControls)
        ]


editColumnPane : List Controller -> Element Msg
editColumnPane subControls =
    column
        [ alignTop
        , padding 10
        , spacing 10
        , editPanelWidth
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
                    List.append subControls [ C.Space ]
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
        , paragraph [ Font.size 18 ] [ text "Listening for MIDI..." ]
        , column
            [ height (px 300)
            , width fill
            , padding 2
            , spacing 4
            , scrollbarY
            , Border.width 2
            , Border.dashed
            ]
            (List.map C.controllerToString subControls
                |> List.map text
            )
        , acceptOrCloseButtons
            "Ok"
            ClosePopUp
            (Just <| FinishedEdit <| C.Column subControls)
        ]


editNotePane : EController.EditNoteState -> Element Msg
editNotePane state =
    column
        [ alignTop
        , padding 10
        , spacing 10
        , editPanelWidth
        , backgroundColour White
        , Border.width 4
        ]
        [ editTextBox
            { placeholder = "label"
            , label = "Label"
            , current = state.label
            }
            "text"
            (\newLabel ->
                { state | label = newLabel }
                    |> EditNote
                    |> UpdateControllerState
            )
        , labelSizeRadio
            state.labelSize
            (\newLabelSize ->
                { state | labelSize = newLabelSize }
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
            "number"
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
            "number"
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
            "number"
            (\newVelocity ->
                { state | velocity = newVelocity }
                    |> EditNote
                    |> UpdateControllerState
            )
        , acceptOrCloseButtons
            "Ok"
            ClosePopUp
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
        , editPanelWidth
        , backgroundColour White
        , Border.width 4
        ]
        [ editTextBox
            { placeholder = "label"
            , label = "Label"
            , current = state.label
            }
            "text"
            (\newLabel ->
                { state | label = newLabel }
                    |> EditChord
                    |> UpdateControllerState
            )
        , labelSizeRadio
            state.labelSize
            (\newLabelSize ->
                { state | labelSize = newLabelSize }
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
            "number"
            (\newVelocity ->
                { state | velocity = newVelocity }
                    |> EditChord
                    |> UpdateControllerState
            )
        , column
            [ width fill
            , Border.width 2
            , Border.dashed
            ]
            [ row [ padding 4, spacing 4, width fill, backgroundColour LightGrey ]
                [ text "Notes"
                , Input.button
                    [ alignRight
                    , padding 5
                    , spacing 2
                    , Border.width 2
                    , Border.solid
                    , borderColour Black
                    , Font.size 14
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
                [ height (px 96)
                , padding 4
                , spacing 2
                , width fill
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
            ClosePopUp
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
        , editPanelWidth
        , backgroundColour White
        , Border.width 4
        ]
        [ editTextBox
            { placeholder = "label"
            , label = "Label"
            , current = state.label
            }
            "text"
            (\newLabel ->
                { state | label = newLabel }
                    |> EditCCValue
                    |> UpdateControllerState
            )
        , labelSizeRadio
            state.labelSize
            (\newLabelSize ->
                { state | labelSize = newLabelSize }
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
            "number"
            (\newChannel ->
                { state | channel = newChannel }
                    |> EditCCValue
                    |> UpdateControllerState
            )
        , editTextBox
            { placeholder = "controller#"
            , label = "Controller\nNumber"
            , current = state.controller
            }
            "number"
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
            "number"
            (\newValue ->
                { state | value = newValue }
                    |> EditCCValue
                    |> UpdateControllerState
            )
        , acceptOrCloseButtons
            "Ok"
            ClosePopUp
            (Maybe.map
                (\c -> FinishedEdit c)
                (EController.editStateToCCValue state)
            )
        ]


editCommandPane : EController.EditCommandState -> Element Msg
editCommandPane state =
    column
        [ alignTop
        , padding 10
        , spacing 10
        , editPanelWidth
        , backgroundColour White
        , Border.width 4
        ]
        [ editTextBox
            { placeholder = "label"
            , label = "Label"
            , current = state.label
            }
            "text"
            (\newLabel ->
                { state | label = newLabel }
                    |> EditCommand
                    |> UpdateControllerState
            )
        , labelSizeRadio
            state.labelSize
            (\newLabelSize ->
                { state | labelSize = newLabelSize }
                    |> EditCommand
                    |> UpdateControllerState
            )
        , colourRadio
            state.colour
            (\newColour ->
                { state | colour = newColour }
                    |> EditCommand
                    |> UpdateControllerState
            )
        , Input.radio
            [ spacing 10 ]
            { onChange =
                \newEditMode ->
                    { state | editMode = newEditMode }
                        |> EditCommand
                        |> UpdateControllerState
            , selected = Just state.editMode
            , label = Input.labelHidden "On Event Type"
            , options =
                [ Input.option EController.OnPressMsgs (text "Button Press")
                , Input.option EController.OnReleaseMsgs (text "Button Release")
                ]
            }
        , case state.newMsg of
            Just newMsg ->
                newCommandMidiMsgView newMsg state

            Nothing ->
                column [ spacing 4, width fill ]
                    [ column
                        [ height (px 300)
                        , width fill
                        , spacing 4
                        , scrollbarY
                        , Border.width 2
                        , Border.dashed
                        ]
                        [ column
                            [ spacing 6
                            , width fill
                            ]
                            (row [ padding 4, spacing 4, width fill, backgroundColour LightGrey ]
                                [ text "Messages"
                                , Input.button
                                    [ alignRight
                                    , padding 5
                                    , Border.width 2
                                    , Border.solid
                                    , borderColour Black
                                    , Font.size 14
                                    ]
                                    { onPress =
                                        Just
                                            ({ state
                                                | newMsg =
                                                    Just <|
                                                        Midi.ENoteOn
                                                            { channel = ""
                                                            , pitch = ""
                                                            , velocity = ""
                                                            }
                                             }
                                                |> EditCommand
                                                |> UpdateControllerState
                                            )
                                    , label = text "Add Msg"
                                    }
                                , Input.button
                                    [ alignRight
                                    , padding 5
                                    , spacing 2
                                    , Border.width 2
                                    , Border.solid
                                    , borderColour Black
                                    , Font.size 14
                                    ]
                                    { onPress =
                                        (case state.editMode of
                                            EController.OnPressMsgs ->
                                                { state | onPressMsgs = [] }

                                            EController.OnReleaseMsgs ->
                                                { state | onReleaseMsgs = [] }
                                        )
                                            |> EditCommand
                                            |> UpdateControllerState
                                            |> Just
                                    , label = text "Clear"
                                    }
                                ]
                                :: ((case state.editMode of
                                        EController.OnPressMsgs ->
                                            state.onPressMsgs

                                        EController.OnReleaseMsgs ->
                                            state.onReleaseMsgs
                                    )
                                        |> List.map
                                            (\midiMsg ->
                                                paragraph []
                                                    [ text <|
                                                        Midi.midiMsgToString midiMsg
                                                    ]
                                            )
                                   )
                            )
                        ]
                    ]
        , acceptOrCloseButtons
            "Ok"
            ClosePopUp
            (Maybe.map
                (\c -> FinishedEdit c)
                (EController.editStateToCommand state)
            )
        ]


newCommandMidiMsgView : Midi.EditMidiButtonMsg -> EController.EditCommandState -> Element Msg
newCommandMidiMsgView newMsg state =
    column
        [ padding 4
        , spacing 4
        , width fill
        , Border.dashed
        , Border.width 2
        ]
        [ Midi.editMidiButtonSelector
            (\msgType ->
                { state
                    | newMsg =
                        Just <|
                            msgType
                }
                    |> EditCommand
                    |> UpdateControllerState
            )
            newMsg
        , Midi.editMidiButtonMsgView
            (\updatedMidi ->
                { state
                    | newMsg =
                        Just <|
                            updatedMidi
                }
                    |> EditCommand
                    |> UpdateControllerState
            )
            newMsg
        , el [ centerX ] <|
            acceptOrCloseButtons
                "Add"
                ({ state
                    | newMsg = Nothing
                 }
                    |> EditCommand
                    |> UpdateControllerState
                )
                (Midi.editMidiButtonToMidiMsg newMsg
                    |> Maybe.map
                        (\completeMsg ->
                            case state.editMode of
                                EController.OnPressMsgs ->
                                    { state
                                        | onPressMsgs =
                                            List.append state.onPressMsgs [ completeMsg ]
                                        , newMsg = Nothing
                                    }
                                        |> EditCommand
                                        |> UpdateControllerState

                                EController.OnReleaseMsgs ->
                                    { state
                                        | onReleaseMsgs =
                                            List.append state.onReleaseMsgs [ completeMsg ]
                                        , newMsg = Nothing
                                    }
                                        |> EditCommand
                                        |> UpdateControllerState
                        )
                )
        ]


editSequencePane : EController.EditSequenceState -> Element Msg
editSequencePane state =
    column
        [ alignTop
        , padding 10
        , spacing 10
        , editPanelWidth
        , backgroundColour White
        , Border.width 4
        ]
        [ editTextBox
            { placeholder = "label"
            , label = "Label"
            , current = state.label
            }
            "text"
            (\newLabel ->
                { state | label = newLabel }
                    |> EditSequence
                    |> UpdateControllerState
            )
        , labelSizeRadio
            state.labelSize
            (\newLabelSize ->
                { state | labelSize = newLabelSize }
                    |> EditSequence
                    |> UpdateControllerState
            )
        , colourRadio
            state.colour
            (\newColour ->
                { state | colour = newColour }
                    |> EditSequence
                    |> UpdateControllerState
            )
        , case state.newMsg of
            Just newMsg ->
                newSequenceMidiMsgView newMsg state

            Nothing ->
                column [ spacing 4, width fill ]
                    [ column
                        [ height (px 300)
                        , width fill
                        , spacing 4
                        , scrollbarY
                        , Border.width 2
                        , Border.dashed
                        ]
                        [ column
                            [ spacing 6
                            , width fill
                            ]
                            (row [ padding 4, spacing 4, width fill, backgroundColour LightGrey ]
                                [ text "Messages"
                                , Input.button
                                    [ alignRight
                                    , padding 5
                                    , Border.width 2
                                    , Border.solid
                                    , borderColour Black
                                    , Font.size 14
                                    ]
                                    { onPress =
                                        Just
                                            ({ state
                                                | newMsg =
                                                    Just <|
                                                        Midi.ENoteOn
                                                            { channel = ""
                                                            , pitch = ""
                                                            , velocity = ""
                                                            }
                                             }
                                                |> EditSequence
                                                |> UpdateControllerState
                                            )
                                    , label = text "Add Msg"
                                    }
                                , Input.button
                                    [ alignRight
                                    , padding 5
                                    , spacing 2
                                    , Border.width 2
                                    , Border.solid
                                    , borderColour Black
                                    , Font.size 14
                                    ]
                                    { onPress =
                                        { state | midiMsgs = Array.empty }
                                            |> EditSequence
                                            |> UpdateControllerState
                                            |> Just
                                    , label = text "Clear"
                                    }
                                ]
                                :: (state.midiMsgs
                                        |> Array.map
                                            (\midiMsg ->
                                                paragraph []
                                                    [ text <|
                                                        Midi.midiMsgToString midiMsg
                                                    ]
                                            )
                                        |> Array.toList
                                   )
                            )
                        ]
                    ]
        , acceptOrCloseButtons
            "Ok"
            ClosePopUp
            (Maybe.map
                (\c -> FinishedEdit c)
                (EController.editStateToSequence state)
            )
        ]


newSequenceMidiMsgView : Midi.EditMidiButtonMsg -> EController.EditSequenceState -> Element Msg
newSequenceMidiMsgView newMsg state =
    column
        [ padding 4
        , spacing 4
        , width fill
        , Border.dashed
        , Border.width 2
        ]
        [ Midi.editMidiButtonSelector
            (\msgType ->
                { state
                    | newMsg =
                        Just <|
                            msgType
                }
                    |> EditSequence
                    |> UpdateControllerState
            )
            newMsg
        , Midi.editMidiButtonMsgView
            (\updatedMidi ->
                { state
                    | newMsg =
                        Just <|
                            updatedMidi
                }
                    |> EditSequence
                    |> UpdateControllerState
            )
            newMsg
        , el [ centerX ] <|
            acceptOrCloseButtons
                "Add"
                ({ state
                    | newMsg = Nothing
                 }
                    |> EditSequence
                    |> UpdateControllerState
                )
                (Midi.editMidiButtonToMidiMsg newMsg
                    |> Maybe.map
                        (\completeMsg ->
                            { state
                                | midiMsgs =
                                    Array.append state.midiMsgs <|
                                        Array.fromList [ completeMsg ]
                                , newMsg = Nothing
                            }
                                |> EditSequence
                                |> UpdateControllerState
                        )
                )
        ]


editFaderPane : EController.EditFaderState -> Element Msg
editFaderPane state =
    column
        [ alignTop
        , padding 10
        , spacing 10
        , editPanelWidth
        , backgroundColour White
        , Border.width 4
        ]
        [ editTextBox
            { placeholder = "label"
            , label = "Label"
            , current = state.label
            }
            "text"
            (\newLabel ->
                { state | label = newLabel }
                    |> EditFader
                    |> UpdateControllerState
            )
        , labelSizeRadio
            state.labelSize
            (\newLabelSize ->
                { state | labelSize = newLabelSize }
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
            "number"
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
            "number"
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
            "number"
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
            "number"
            (\newMaxValue ->
                { state | valueMax = newMaxValue }
                    |> EditFader
                    |> UpdateControllerState
            )
        , acceptOrCloseButtons
            "Ok"
            ClosePopUp
            (Maybe.map
                (\c -> FinishedEdit c)
                (EController.editStateToFader state)
            )
        ]


editXYFaderPane : EController.EditXYFaderState -> Element Msg
editXYFaderPane state =
    column
        [ alignTop
        , padding 10
        , spacing 10
        , editPanelWidth
        , backgroundColour White
        , Border.width 4
        ]
        [ editTextBox
            { placeholder = "label"
            , label = "Label"
            , current = state.label
            }
            "text"
            (\newLabel ->
                { state | label = newLabel }
                    |> EditXYFader
                    |> UpdateControllerState
            )
        , labelSizeRadio
            state.labelSize
            (\newLabelSize ->
                { state | labelSize = newLabelSize }
                    |> EditXYFader
                    |> UpdateControllerState
            )
        , colourRadio
            state.colour
            (\newColour ->
                { state | colour = newColour }
                    |> EditXYFader
                    |> UpdateControllerState
            )
        , Input.radio
            [ spacing 10 ]
            { onChange =
                \newActive ->
                    { state | active = newActive }
                        |> EditXYFader
                        |> UpdateControllerState
            , selected = Just state.active
            , label = Input.labelHidden "Param Select"
            , options =
                [ Input.option EController.Params1 (text "Params X")
                , Input.option EController.Params2 (text "Params Y")
                ]
            }
        , case state.active of
            EController.Params1 ->
                column [ padding 5, Border.width 2, Border.dashed ]
                    [ editTextBox
                        { placeholder = "channel x#"
                        , label = "Channel X"
                        , current = state.channel1
                        }
                        "number"
                        (\newChannel ->
                            { state | channel1 = newChannel }
                                |> EditXYFader
                                |> UpdateControllerState
                        )
                    , editTextBox
                        { placeholder = "cc x #"
                        , label = "CC X Number"
                        , current = state.ccNumber1
                        }
                        "number"
                        (\newCCNumber ->
                            { state | ccNumber1 = newCCNumber }
                                |> EditXYFader
                                |> UpdateControllerState
                        )
                    , editTextBox
                        { placeholder = "x min value"
                        , label = "X Min Value"
                        , current = state.valueMin1
                        }
                        "number"
                        (\newMinValue ->
                            { state | valueMin1 = newMinValue }
                                |> EditXYFader
                                |> UpdateControllerState
                        )
                    , editTextBox
                        { placeholder = "x max value"
                        , label = "X Max Value"
                        , current = state.valueMax1
                        }
                        "number"
                        (\newMaxValue ->
                            { state | valueMax1 = newMaxValue }
                                |> EditXYFader
                                |> UpdateControllerState
                        )
                    ]

            EController.Params2 ->
                column [ padding 5, Border.width 2, Border.dashed ]
                    [ editTextBox
                        { placeholder = "channel y#"
                        , label = "Channel Y"
                        , current = state.channel2
                        }
                        "number"
                        (\newChannel ->
                            { state | channel2 = newChannel }
                                |> EditXYFader
                                |> UpdateControllerState
                        )
                    , editTextBox
                        { placeholder = "cc y #"
                        , label = "CC Y Number"
                        , current = state.ccNumber2
                        }
                        "number"
                        (\newCCNumber ->
                            { state | ccNumber2 = newCCNumber }
                                |> EditXYFader
                                |> UpdateControllerState
                        )
                    , editTextBox
                        { placeholder = "y min value"
                        , label = "Y Min Value"
                        , current = state.valueMin2
                        }
                        "number"
                        (\newMinValue ->
                            { state | valueMin2 = newMinValue }
                                |> EditXYFader
                                |> UpdateControllerState
                        )
                    , editTextBox
                        { placeholder = "y max value"
                        , label = "Y Max Value"
                        , current = state.valueMax2
                        }
                        "number"
                        (\newMaxValue ->
                            { state | valueMax2 = newMaxValue }
                                |> EditXYFader
                                |> UpdateControllerState
                        )
                    ]
        , acceptOrCloseButtons
            "Ok"
            ClosePopUp
            (Maybe.map
                (\c -> FinishedEdit c)
                (EController.editStateToXYFader state)
            )
        ]


editPitchBendPane : EController.EditPitchBendState -> Element Msg
editPitchBendPane state =
    column
        [ alignTop
        , padding 10
        , spacing 10
        , editPanelWidth
        , backgroundColour White
        , Border.width 4
        ]
        [ editTextBox
            { placeholder = "label"
            , label = "Label"
            , current = state.label
            }
            "text"
            (\newLabel ->
                { state | label = newLabel }
                    |> EditPitchBend
                    |> UpdateControllerState
            )
        , labelSizeRadio
            state.labelSize
            (\newLabelSize ->
                { state | labelSize = newLabelSize }
                    |> EditPitchBend
                    |> UpdateControllerState
            )
        , colourRadio
            state.colour
            (\newColour ->
                { state | colour = newColour }
                    |> EditPitchBend
                    |> UpdateControllerState
            )
        , editTextBox
            { placeholder = "channel#"
            , label = "Channel"
            , current = state.channel
            }
            "number"
            (\newChannel ->
                { state | channel = newChannel }
                    |> EditPitchBend
                    |> UpdateControllerState
            )
        , acceptOrCloseButtons
            "Ok"
            ClosePopUp
            (Maybe.map
                (\c -> FinishedEdit c)
                (EController.editStateToPitchBend state)
            )
        ]



-- }}}
-- {{{ New/Edit Page Menu


newPageMenu : Dict String Page -> PageMenuState -> Element Msg
newPageMenu savedPages state =
    el [ centerX, centerY ] <|
        column
            [ padding 10
            , spacing 10
            , backgroundColour White
            , Border.width 4
            ]
            [ paragraph [ Font.bold ] [ text "Pages" ]
            , el [ centerX ] <|
                Input.radioRow
                    [ spacing 10 ]
                    { onChange =
                        \newMode ->
                            { state | mode = newMode }
                                |> UpdatePageMenuState
                    , selected = Just state.mode
                    , label = Input.labelHidden "Mode"
                    , options =
                        [ Input.option NewPage (text "New")
                        , Input.option LoadPage (text "Load")
                        , Input.option ImportPage (text "Import")
                        ]
                    }
            , case state.mode of
                NewPage ->
                    Input.text
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

                LoadPage ->
                    column [ spacing 10 ]
                        [ case state.mSelectedPage of
                            Just index ->
                                Input.button
                                    [ padding 5
                                    , Border.width 2
                                    , Border.solid
                                    , backgroundColour Red
                                    , fontColour White
                                    , borderColour Black
                                    ]
                                    { onPress =
                                        Dict.toList savedPages
                                            |> Array.fromList
                                            |> Array.get index
                                            |> Maybe.map (\( key, _ ) -> DeleteSavedPage key)
                                    , label = text "Delete"
                                    }

                            Nothing ->
                                none
                        , column
                            [ height (px 200)
                            , width fill
                            , scrollbarY
                            , Border.width 2
                            , Border.dashed
                            ]
                            (Dict.keys savedPages
                                |> List.indexedMap
                                    (selectableOption
                                        (\newSelected ->
                                            { state | mSelectedPage = Just newSelected }
                                                |> UpdatePageMenuState
                                        )
                                        state.mSelectedPage
                                    )
                            )
                        , el [ centerX ] <|
                            Input.text
                                [ Border.width 2
                                , Border.rounded 0
                                , borderColour Black
                                , htmlAttribute <| HAtt.type_ "number"
                                ]
                                { onChange =
                                    \newChannel ->
                                        { state
                                            | bulkEditChannel =
                                                if String.isEmpty newChannel then
                                                    Nothing

                                                else
                                                    Just newChannel
                                        }
                                            |> UpdatePageMenuState
                                , text =
                                    state.bulkEditChannel
                                        |> Maybe.withDefault ""
                                , placeholder =
                                    Just <|
                                        Input.placeholder []
                                            (text "set channel")
                                , label = Input.labelAbove [] (text "Set Channel (Optional)")
                                }
                        ]

                ImportPage ->
                    column
                        [ spacing 10 ]
                        [ Input.button
                            [ padding 5
                            , Border.width 2
                            , Border.solid
                            ]
                            { onPress = Just ImportPageRequested
                            , label = text "Import Page"
                            }
                        , case ( state.mImportedPage, state.mImportError ) of
                            ( Just _, Nothing ) ->
                                paragraph [] [ text <| "Import Sucessful" ]

                            ( Nothing, Just error ) ->
                                paragraph [] [ text <| "Failed to import: " ++ error ]

                            _ ->
                                none
                        ]
            , acceptOrCloseButtons
                "Add Page"
                ClosePopUp
                (case state.mode of
                    NewPage ->
                        if String.isEmpty state.label then
                            Nothing

                        else
                            { label = state.label
                            , controller = C.Space
                            , config =
                                { gapSize = 2
                                , debug = False
                                }
                            }
                                |> AddPage
                                |> Just

                    LoadPage ->
                        state.mSelectedPage
                            |> Maybe.andThen
                                (\i ->
                                    Dict.values savedPages
                                        |> Array.fromList
                                        |> Array.get i
                                        |> Maybe.map
                                            (\p ->
                                                case
                                                    Maybe.andThen
                                                        Midi.stringToChannel
                                                        state.bulkEditChannel
                                                of
                                                    Just ch ->
                                                        { p
                                                            | controller =
                                                                setChannel ch p.controller
                                                        }
                                                            |> AddPage

                                                    Nothing ->
                                                        AddPage p
                                            )
                                )

                    ImportPage ->
                        Maybe.map
                            (\p -> AddPage p)
                            state.mImportedPage
                )
            ]


importPageFromUrlMenu : String -> Element Msg
importPageFromUrlMenu pageString =
    let
        mDecompressedPage =
            pageString
                |> Base64.toBytes
                |> Maybe.andThen Flate.inflateGZip
                |> Maybe.andThen
                    (\bytes ->
                        Decode.decode (Decode.string (Bytes.width bytes)) bytes
                    )
                |> Maybe.withDefault ""
                |> Codec.decodeString pageCodec
                |> Result.toMaybe
    in
    el [ centerX, centerY ] <|
        column
            [ padding 10
            , spacing 10
            , width <| px 300
            , backgroundColour White
            , Border.width 4
            ]
            [ paragraph [ Font.bold ] [ text "Import Page From URL" ]
            , case mDecompressedPage of
                Just page ->
                    paragraph
                        []
                        [ text <|
                            "Do you want to import \""
                                ++ page.label
                                ++ "\" to your pages?"
                        ]

                Nothing ->
                    paragraph []
                        [ """Could not decompress page in URL, 
                          make sure you have copied it correctly.
                          """
                            |> text
                        ]
            , acceptOrCloseButtons
                "Add Page"
                ClosePopUp
                (Maybe.map AddPage mDecompressedPage)
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
        ((htmlAttribute <| Utils.onContextMenu NoOp)
            :: fillSpace
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
        C.Module label subControls ->
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
                                            C.EditContainer
                                            (updatedParts
                                                |> List.reverse
                                                |> String.join "_"
                                            )
                                    ]
                           )
                    )
                , renderController mode midiLog config updatedParts subControls 0
                ]

        C.Row subControls ->
            Lazy.lazy2 row
                ([ spacingXY config.gapSize 0
                 , Events.onClick NoOp
                 ]
                    ++ fillSpace
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
                                        C.EditContainer
                                        (updatedParts
                                            |> List.reverse
                                            |> String.join "_"
                                        )
                                ]
                       )
                )

        C.Column subControls ->
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
                            C.EditContainer
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

        C.Note state ->
            renderNote
                config
                mode
                state
                (updatedParts
                    |> List.reverse
                    |> String.join "_"
                )

        C.Chord state ->
            renderChord
                config
                mode
                state
                (updatedParts
                    |> List.reverse
                    |> String.join "_"
                )

        C.CCValue state ->
            renderCCValue
                config
                mode
                state
                (updatedParts
                    |> List.reverse
                    |> String.join "_"
                )

        C.Command state ->
            renderCommand
                config
                mode
                state
                (updatedParts
                    |> List.reverse
                    |> String.join "_"
                )

        C.Sequence state ->
            renderSequence
                config
                mode
                state
                (updatedParts
                    |> List.reverse
                    |> String.join "_"
                )

        C.Fader state ->
            renderFader
                config
                mode
                state
                (updatedParts
                    |> List.reverse
                    |> String.join "_"
                )

        C.XYFader state ->
            renderXYFader
                config
                mode
                state
                (updatedParts
                    |> List.reverse
                    |> String.join "_"
                )

        C.PitchBend state ->
            renderPitchBend
                config
                mode
                state
                (updatedParts
                    |> List.reverse
                    |> String.join "_"
                )

        C.MidiLog ->
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

        C.Space ->
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


renderNote : PageConfig -> Mode -> C.NoteState -> String -> Element Msg
renderNote config mode state id =
    case mode of
        Normal ->
            el
                ([ padding 0
                 , spacing 0
                 , Border.width 4
                 , case state.status of
                    C.Off ->
                        backgroundColour state.colour

                    C.On ->
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
                        , labelSizeToFontSize <| Maybe.withDefault Small state.labelSize
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


renderChord : PageConfig -> Mode -> C.ChordState -> String -> Element Msg
renderChord config mode state id =
    case mode of
        Normal ->
            el
                ([ padding 0
                 , spacing 0
                 , Border.width 4
                 , case state.status of
                    C.Off ->
                        backgroundColour state.colour

                    C.On ->
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
                        , labelSizeToFontSize <| Maybe.withDefault Small state.labelSize
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


renderCCValue : PageConfig -> Mode -> C.CCValueState -> String -> Element Msg
renderCCValue config mode state id =
    case mode of
        Normal ->
            el
                ([ padding 0
                 , spacing 0
                 , Border.width 4
                 , case state.status of
                    C.Off ->
                        backgroundColour state.colour

                    C.On ->
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
                        C.Off ->
                            "Off\n" ++ Midi.channelToString state.channel ++ "\n"

                        C.On ->
                            "Off\n" ++ Midi.channelToString state.channel ++ "\n"

                  else
                    ""
                 )
                    ++ state.label
                    |> text
                    |> el
                        [ centerX
                        , centerY
                        , labelSizeToFontSize <| Maybe.withDefault Small state.labelSize
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


renderCommand : PageConfig -> Mode -> C.CommandState -> String -> Element Msg
renderCommand config mode state id =
    case mode of
        Normal ->
            el
                ([ padding 0
                 , spacing 0
                 , Border.width 4
                 , case state.status of
                    C.Off ->
                        backgroundColour state.colour

                    C.On ->
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
                        , labelSizeToFontSize <| Maybe.withDefault Small state.labelSize
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


renderSequence : PageConfig -> Mode -> C.SequenceState -> String -> Element Msg
renderSequence config mode state id =
    case mode of
        Normal ->
            el
                ([ padding 0
                 , spacing 0
                 , Border.width 4
                 , case state.status of
                    C.Off ->
                        backgroundColour state.colour

                    C.On ->
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
                    ++ "\n"
                    ++ String.fromInt (state.index + 1)
                    |> text
                    |> el
                        [ centerX
                        , centerY
                        , labelSizeToFontSize <| Maybe.withDefault Small state.labelSize
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


renderFader : PageConfig -> Mode -> C.FaderState -> String -> Element Msg
renderFader config mode state id =
    case mode of
        Normal ->
            el
                ([ padding 0
                 , spacing 0
                 , Border.width 4
                 , case state.status of
                    C.Set ->
                        Border.solid

                    C.Changing _ _ ->
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
                    [ el [ centerX, padding 10, Font.size 14 ] <|
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
                    , el
                        [ centerX
                        , padding 10
                        , labelSizeToFontSize <| Maybe.withDefault Small state.labelSize
                        ]
                      <|
                        text state.label
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


renderXYFader : PageConfig -> Mode -> C.XYFaderState -> String -> Element Msg
renderXYFader config mode state id =
    case mode of
        Normal ->
            el
                ([ padding 0
                 , spacing 0
                 , Border.width 4
                 , case state.status of
                    C.Set ->
                        Border.solid

                    C.Changing _ _ ->
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
                    [ el [ centerX, padding 10, Font.size 14 ] <|
                        text
                            ("X "
                                ++ String.fromInt state.valuePercent1
                                ++ "%, "
                                ++ "Y "
                                ++ String.fromInt state.valuePercent2
                                ++ "%"
                            )
                    , column
                        ([ inFront <|
                            row
                                fillSpace
                                [ el
                                    [ height fill
                                    , width <| fillPortion state.valuePercent1
                                    ]
                                    none
                                , el
                                    [ height fill
                                    , width <| fillPortion (100 - state.valuePercent1)
                                    , Border.widthEach { bottom = 0, top = 0, left = 8, right = 0 }
                                    ]
                                    none
                                ]
                         , backgroundColour state.colour
                         ]
                            ++ fillSpace
                        )
                        [ el
                            [ height <| fillPortion (100 - state.valuePercent2)
                            , width fill
                            ]
                            none
                        , el
                            [ height <| fillPortion state.valuePercent2
                            , width fill
                            , Border.widthEach { bottom = 0, top = 8, left = 0, right = 0 }
                            ]
                            none
                        ]
                    , el
                        [ centerX
                        , padding 10
                        , labelSizeToFontSize <| Maybe.withDefault Small state.labelSize
                        ]
                      <|
                        text state.label
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


renderPitchBend : PageConfig -> Mode -> C.PitchBendState -> String -> Element Msg
renderPitchBend config mode state id =
    let
        fillFraction =
            round <| 100 * (toFloat state.bendValue / 16383)
    in
    case mode of
        Normal ->
            el
                ([ padding 0
                 , spacing 0
                 , Border.width 4
                 , case state.status of
                    C.Set ->
                        Border.solid

                    C.Changing _ _ ->
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
                    [ column
                        fillSpace
                        [ el
                            [ height <| fillPortion (100 - fillFraction)
                            , width fill
                            , backgroundColour state.colour
                            ]
                            none
                        , el
                            [ height <| fillPortion fillFraction
                            , width fill
                            , backgroundColour state.colour
                            , Border.widthEach { bottom = 0, top = 8, left = 0, right = 0 }
                            ]
                            none
                        ]
                    , el
                        [ centerX
                        , padding 10
                        , labelSizeToFontSize <| Maybe.withDefault Small state.labelSize
                        ]
                      <|
                        text state.label
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


renderEditButton : PageConfig -> C.EditOperation -> String -> Element Msg
renderEditButton config editOperation parentId =
    case editOperation of
        C.EditContainer ->
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

        C.Add ->
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

        C.Remove ->
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
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = \_ -> NoOp
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.midiDevices MidiDevicesChanged
        , Ports.incomingMidi IncomingMidi
        , Browser.Events.onResize PageResized
        ]



-- }}}
