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
import EditableController as EC exposing (EditableController(..))
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
import Music.Chord as Chord
import Music.ChordType as ChordType
import Music.Interval as Interval exposing (Interval)
import Music.Pitch as Pitch
import Music.PitchClass as PitchClass exposing (PitchClass)
import Music.Range as Range
import Music.Scale as Scale
import Music.ScaleType as ScaleType
import Music.Voicing.FourPart as FourPart
import Music.Voicing.ThreePart as ThreePart
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
    "0.3.4"


date : String
date =
    "2023-03-05"



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
    | Edit ( Maybe Controller, List Controller )


type PopUp
    = NoWebMidiPanel String
    | InfoPanel
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
    , config : Utils.PageConfig
    }


pageCodec : Codec Page
pageCodec =
    Codec.object Page
        |> Codec.field "label" .label Codec.string
        |> Codec.field "controller" .controller C.controllerCodec
        |> Codec.field "config" .config Utils.pageConfigCodec
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
              , popup =
                    case mPageString of
                        Just (Just pageString) ->
                            Just <| ImportPageFromUrl pageString

                        _ ->
                            Just InfoPanel
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


scaleToScaleConstructor : EC.Scale -> (PitchClass.PitchClass -> Scale.Scale)
scaleToScaleConstructor scaleId =
    case scaleId of
        EC.Major ->
            Scale.major

        EC.NaturalMinor ->
            Scale.minor

        EC.Ionian ->
            Scale.ionian

        EC.Dorian ->
            Scale.dorian

        EC.Phrygian ->
            Scale.phrygian

        EC.Lydian ->
            Scale.lydian

        EC.Mixolydian ->
            Scale.mixolydian

        EC.Aeolian ->
            Scale.aeolian

        EC.Locrian ->
            Scale.locrian

        EC.MelodicMinor ->
            Scale.melodicMinor

        EC.DorianFlat2 ->
            Scale.dorianFlat2

        EC.LydianAugmented ->
            Scale.lydianAugmented

        EC.Acoustic ->
            Scale.acoustic

        EC.MajorMinor ->
            Scale.majorMinor

        EC.MinorLocrian ->
            Scale.minorLocrian

        EC.SuperLocrian ->
            Scale.superlocrian

        EC.HarmonicMinor ->
            Scale.harmonicMinor

        EC.LocrianNatural6 ->
            Scale.locrianNatural6

        EC.MajorAugmented ->
            Scale.majorAugmented

        EC.LydianDiminished ->
            Scale.lydianDiminished

        EC.PhrygianDominant ->
            Scale.phrygianDominant

        EC.AeolianHarmonic ->
            Scale.aeolianHarmonic

        EC.UltraLocrian ->
            Scale.ultralocrian

        EC.DiminishedWholeToneHalfTone ->
            Scale.diminishedWholeToneHalfTone

        EC.DiminishedHalfToneWholeTone ->
            Scale.diminishedHalfToneWholeTone

        EC.WholeTone ->
            Scale.wholeTone

        EC.MajorPentatonic ->
            Scale.majorPentatonic

        EC.MinorPentatonic ->
            Scale.minorPentatonic


keyToPitchClass : EC.Key -> PitchClass.PitchClass
keyToPitchClass key =
    case key of
        EC.C ->
            PitchClass.c

        EC.Cs ->
            PitchClass.cSharp

        EC.D ->
            PitchClass.d

        EC.Ds ->
            PitchClass.dSharp

        EC.Db ->
            PitchClass.dFlat

        EC.E ->
            PitchClass.e

        EC.Eb ->
            PitchClass.eFlat

        EC.F ->
            PitchClass.f

        EC.Fs ->
            PitchClass.fSharp

        EC.G ->
            PitchClass.g

        EC.Gs ->
            PitchClass.gSharp

        EC.Gb ->
            PitchClass.gFlat

        EC.A ->
            PitchClass.a

        EC.As ->
            PitchClass.aSharp

        EC.Ab ->
            PitchClass.aFlat

        EC.B ->
            PitchClass.b

        EC.Bb ->
            PitchClass.bFlat


scaleKeyboard :
    { channel : Midi.Channel
    , velocity : Int
    , key : EC.Key
    , scaleId : EC.Scale
    , octave : Int
    , range : Int
    }
    -> Controller
scaleKeyboard { channel, velocity, key, scaleId, octave, range } =
    let
        pitchClass =
            keyToPitchClass key

        scaleConstructor =
            scaleToScaleConstructor scaleId

        scale =
            scaleConstructor pitchClass

        intervals =
            Scale.scaleType scale
                |> ScaleType.toList

        octaveNumbers =
            List.range 0 (range - 1)
                |> List.map ((+) octave)
    in
    octaveNumbers
        |> List.map (makeNotesRow channel velocity intervals pitchClass)
        -- This is to make the octaves ascend on the app
        |> List.reverse
        |> C.Column


makeNotesRow : Midi.Channel -> Int -> List Interval -> PitchClass -> Int -> Controller
makeNotesRow channel velocity intervals root octave =
    intervals
        |> List.map (\i -> Pitch.transposeUp i (Pitch.fromPitchClassInOctave octave root))
        |> List.map (\p -> ( Pitch.toString p, Pitch.toMIDINoteNumber p ))
        |> List.filter (\( _, n ) -> List.member n (List.range 1 127))
        |> List.map
            (\( s, n ) -> C.newNote s Medium Style.Green channel n velocity)
        |> C.Row


chordKeyboard :
    { channel : Midi.Channel
    , velocity : Int
    , key : EC.Key
    , scaleId : EC.Scale
    , octave : Int
    , range : Int
    , chordType : EC.ChordType
    }
    -> Controller
chordKeyboard { channel, velocity, key, scaleId, octave, range, chordType } =
    let
        pitchClass =
            keyToPitchClass key

        scaleConstructor =
            scaleToScaleConstructor scaleId

        minPitch =
            Pitch.fromPitchClassInOctave octave pitchClass

        maxPitch =
            Pitch.fromPitchClassInOctave (octave + range) pitchClass

        chords =
            case chordType of
                EC.Triad ->
                    Scale.allChords ChordType.triads (scaleConstructor pitchClass)

                EC.FourNote ->
                    Scale.allChords ChordType.sixthAndSeventhChords (scaleConstructor pitchClass)

        chordButtons =
            case chordType of
                EC.Triad ->
                    List.map (makeTriadChordButtons channel velocity minPitch maxPitch) chords

                EC.FourNote ->
                    List.map (makeJazzChordButtons channel velocity minPitch maxPitch) chords

        longest =
            List.map List.length chordButtons
                |> List.maximum
                |> Maybe.withDefault 1

        paddedChords =
            List.map
                (\cs ->
                    if List.length cs < longest then
                        cs ++ [ C.Space ]

                    else
                        cs
                )
                chordButtons
                |> List.map List.reverse
    in
    List.map C.Column paddedChords
        |> C.Row


makeTriadChordButtons : Midi.Channel -> Int -> Pitch.Pitch -> Pitch.Pitch -> Chord.Chord -> List Controller
makeTriadChordButtons channel velocity minPitch inMaxPitch chord =
    let
        maxPitch =
            Pitch.transposeUp Interval.perfectFourth inMaxPitch

        voicings =
            Chord.voiceThreeParts
                { voiceOne = Range.range minPitch maxPitch
                , voiceTwo = Range.range minPitch maxPitch
                , voiceThree = Range.range minPitch maxPitch
                }
                [ ThreePart.basic ]
                chord

        midiNumberList =
            List.map ThreePart.toPitchList voicings
                |> List.map (List.map Pitch.toMIDINoteNumber)

        chordHelper midiNotes =
            C.newChord (Chord.toString chord) Small LightGrey velocity (List.map (\p -> { channel = channel, pitch = p }) midiNotes)

        chordButtons =
            List.map chordHelper midiNumberList
    in
    chordButtons


makeJazzChordButtons : Midi.Channel -> Int -> Pitch.Pitch -> Pitch.Pitch -> Chord.Chord -> List Controller
makeJazzChordButtons channel velocity minPitch inMaxPitch chord =
    let
        maxPitch =
            Pitch.transposeUp Interval.majorSeventh inMaxPitch

        voicings =
            Chord.voiceFourParts
                { voiceOne = Range.range minPitch maxPitch
                , voiceTwo = Range.range minPitch maxPitch
                , voiceThree = Range.range minPitch maxPitch
                , voiceFour = Range.range minPitch maxPitch
                }
                [ FourPart.basic ]
                chord

        midiNumberList =
            List.map FourPart.toPitchList voicings
                |> List.map (List.map Pitch.toMIDINoteNumber)

        chordHelper midiNotes =
            C.newChord (Chord.toString chord) Small LightGrey velocity (List.map (\p -> { channel = channel, pitch = p }) midiNotes)

        chordButtons =
            List.map chordHelper midiNumberList
    in
    chordButtons



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
                    , valueMax1 = 127
                    , channel2 = Midi.Ch6
                    , ccNumber2 = 2
                    , valuePercent2 = 50
                    , valueMin2 = 0
                    , valueMax2 = 127
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
                    , valueMax1 = 127
                    , channel2 = Midi.Ch6
                    , ccNumber2 = 4
                    , valuePercent2 = 50
                    , valueMin2 = 0
                    , valueMax2 = 127
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
                    , valueMax = 127
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
                    , valueMax = 127
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
                    , valueMax = 127
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
                    , valueMax = 127
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
    = WebMidiNotAvailable String
    | MidiDevicesChanged (List Midi.Device)
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
    | UndoEdit
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
        WebMidiNotAvailable reason ->
            ( { model
                | popup =
                    Just <|
                        NoWebMidiPanel reason
              }
            , Cmd.none
            )

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
                            Edit
                                ( Array.get model.activePage model.pages
                                    |> Maybe.map .controller
                                , []
                                )

                        Edit _ ->
                            Normal
              }
            , Cmd.none
            )

        UndoEdit ->
            ( case model.mode of
                Edit ( first, next :: rest ) ->
                    { model
                        | pages =
                            updateControllerOnActivePage
                                model.activePage
                                { id = "0", updateFn = \_ -> next }
                                model.pages
                        , mode = Edit ( first, rest )
                    }

                Edit ( Just first, [] ) ->
                    { model
                        | pages =
                            updateControllerOnActivePage
                                model.activePage
                                { id = "0", updateFn = \_ -> first }
                                model.pages
                        , mode = Edit ( Just first, [] )
                    }

                Edit ( Nothing, _ ) ->
                    model

                Normal ->
                    model
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
                        , mode = Normal
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
                        , mode = Edit ( Just page.controller, [] )
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
                        Just (EditMenu id oldController) ->
                            (case ( oldController, editType ) of
                                ( EditColumn items, EditRow _ ) ->
                                    EditRow items

                                ( EditColumn items, EditColumn _ ) ->
                                    EditColumn items

                                ( EditRow items, EditRow _ ) ->
                                    EditRow items

                                ( EditRow items, EditColumn _ ) ->
                                    EditColumn items

                                _ ->
                                    editType
                            )
                                |> EditMenu id
                                |> Just

                        _ ->
                            Nothing
            in
            ( { model | popup = newPopup }, Cmd.none )

        FinishedEdit controller ->
            case model.popup of
                Just (EditMenu id _) ->
                    let
                        oldController =
                            getControllerFromActivePage "0" model.activePage model.pages

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
                                        Edit ( Just first, rest ) ->
                                            Edit
                                                ( Just first
                                                , case oldController of
                                                    Just oc ->
                                                        oc :: rest

                                                    Nothing ->
                                                        rest
                                                )

                                        Edit ( Nothing, _ ) ->
                                            Edit ( oldController, [] )

                                        Normal ->
                                            Normal
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

                Just (C.SizedColumn _) ->
                    ( model, Cmd.none )

                Just (C.SizedRow _) ->
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

                Just (C.SizedColumn _) ->
                    ( model, Cmd.none )

                Just (C.SizedRow _) ->
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
                            EC.updateWithMidiMsg midiMsg state
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
                , createMode = EC.New
                , selectedModule = Nothing
                }

        C.Row subControls ->
            EditRow subControls

        C.Column subControls ->
            EditColumn subControls

        C.SizedRow subControls ->
            EditSizedRow subControls

        C.SizedColumn subControls ->
            EditSizedColumn subControls

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
                , editMode = EC.OnPressMsgs
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
                , active = EC.Params1
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
                    case mode of
                        Normal ->
                            defaultButtons

                        Edit _ ->
                            editButtons

                else
                    []
               )
        )


defaultButtons : List (Element Msg)
defaultButtons =
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
            , backgroundColour White
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


editButtons : List (Element Msg)
editButtons =
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
            { onPress = Just ToggleNormalEdit
            , label =
                Icons.x
                    |> Icons.withSize 28
                    |> Icons.toHtml []
                    |> html
            }
        , Input.button
            [ padding 10
            , Border.width 4
            , backgroundColour White
            ]
            { onPress = Just UndoEdit
            , label =
                Icons.rotateCcw
                    |> Icons.withSize 28
                    |> Icons.toHtml []
                    |> html
            }
        ]
    ]


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
            NoWebMidiPanel reason ->
                noWebMidiPanel reason

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


noWebMidiPanel : String -> Element Msg
noWebMidiPanel _ =
    el [ centerX, centerY ] <|
        column
            [ centerX
            , padding 20
            , spacing 20
            , backgroundColour White
            , Border.width 4
            ]
            [ textColumn
                [ spacing 20, Font.size 16, width <| px 300 ]
                [ paragraph [ Font.size 24, Font.bold ]
                    [ text <|
                        """Web MIDI API is not available!
                        """
                    ]
                , paragraph []
                    [ text <|
                        """I can't access the Web MIDI API on your browser. You
                        could try refreshing your page, but this is probably
                        happening because either you need to allow access (there
                        should have had a popup) or your browser does not
                        support the Web MIDI API. Please check the following
                        website to determine if your browser is compatible:
                        """
                    , newTabLink
                        linkStyle
                        { url = "https://caniuse.com/midi"
                        , label = text "https://caniuse.com/midi"
                        }
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
            ]



-- }}}
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
                        """The "No Wasted Effort" is here, you can now share your 
                        presets with a URL! Scroll down for details.
                        """
                    ]
                , Html.iframe
                    [ HAtt.height 300
                    , HAtt.width 360
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
                    [ """Please consider supporting the development of this
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
                    [ el [ Font.italic ] <| text "The No Wasted Effort Update (2022-01-30): "
                    , text <|
                        """This update is all about saving and sharing your presets. You can
                        share your presets with a URL, and the cool thing
                        is that all the data for the preset is encoded in the link! You can also
                        export your pages to files to back them up or move them to another device.
                        """
                    ]
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
            [ column
                []
                [ selectorOption
                    menuType
                    (EditModule
                        { label = ""
                        , controller = C.Space
                        , createMode = EC.New
                        , selectedModule = Nothing
                        }
                    )
                , selectorOption
                    menuType
                    (EditIsomorphic EC.defaultEditIsomorphicState)
                , selectorOption
                    menuType
                    (EditScale EC.defaultEditScaleState)
                , selectorOption
                    menuType
                    (EditChordBuilder EC.defaultEditChordBuilderState)
                , selectorOption menuType (EditSizedColumn [])
                , selectorOption menuType (EditSizedRow [])
                , selectorOption
                    menuType
                    (EditNote EC.defaultEditNoteState)
                , selectorOption
                    menuType
                    (EditChord EC.defaultEditChordState)
                , selectorOption
                    menuType
                    (EditCCValue EC.defaultEditCCValueState)
                , selectorOption
                    menuType
                    (EditCommand EC.defaultEditCommandState)
                , selectorOption
                    menuType
                    (EditSequence EC.defaultEditSequenceState)
                , selectorOption
                    menuType
                    (EditFader EC.defaultEditFaderState)
                , selectorOption
                    menuType
                    (EditXYFader EC.defaultEditXYFaderState)
                , selectorOption
                    menuType
                    (EditPitchBend EC.defaultEditPitchBendState)
                , selectorOption menuType EditMidiLog
                , selectorOption menuType EditSpace
                ]
            , paragraph
                [ alignTop
                , height fill
                , padding 10
                , Border.width 2
                , Border.dashed
                , Font.size 14
                , Font.alignLeft
                ]
                [ text <| EC.description menuType ]
            ]
        , case menuType of
            EditModule state ->
                editModulePane savedModules state

            EditIsomorphic state ->
                editIsomorphicPane state

            EditScale state ->
                editScalePane state

            EditChordBuilder state ->
                editChordBuilderPane state

            EditRow subControls ->
                editRowPane subControls

            EditColumn subControls ->
                editColumnPane subControls

            EditSizedRow subControls ->
                editSizedRowPane subControls

            EditSizedColumn subControls ->
                editSizedColumnPane subControls

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


selectorOption : EditableController -> EditableController -> Element Msg
selectorOption current new =
    el
        (fillSpace
            ++ [ padding 5
               , Events.onClick <|
                    SetEditType new
               ]
            ++ (case ( current, new ) of
                    ( EditModule _, EditModule _ ) ->
                        [ backgroundColour Blue ]

                    ( EditIsomorphic _, EditIsomorphic _ ) ->
                        [ backgroundColour Blue ]

                    ( EditScale _, EditScale _ ) ->
                        [ backgroundColour Blue ]

                    ( EditChordBuilder _, EditChordBuilder _ ) ->
                        [ backgroundColour Blue ]

                    ( EditColumn _, EditColumn _ ) ->
                        [ backgroundColour Blue ]

                    ( EditRow _, EditRow _ ) ->
                        [ backgroundColour Blue ]

                    ( EditNote _, EditNote _ ) ->
                        [ backgroundColour Blue ]

                    ( EditChord _, EditChord _ ) ->
                        [ backgroundColour Blue ]

                    ( EditCCValue _, EditCCValue _ ) ->
                        [ backgroundColour Blue ]

                    ( EditCommand _, EditCommand _ ) ->
                        [ backgroundColour Blue ]

                    ( EditSequence _, EditSequence _ ) ->
                        [ backgroundColour Blue ]

                    ( EditFader _, EditFader _ ) ->
                        [ backgroundColour Blue ]

                    ( EditXYFader _, EditXYFader _ ) ->
                        [ backgroundColour Blue ]

                    ( EditPitchBend _, EditPitchBend _ ) ->
                        [ backgroundColour Blue ]

                    ( EditMidiLog, EditMidiLog ) ->
                        [ backgroundColour Blue ]

                    ( EditSpace, EditSpace ) ->
                        [ backgroundColour Blue ]

                    ( _, _ ) ->
                        []
               )
        )
        (text <| EC.editableControllerToString new)


editModulePane : Dict String Controller -> EC.EditModuleState -> Element Msg
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
                        |> EC.EditModule
                        |> UpdateControllerState
            , selected = Just state.createMode
            , label = Input.labelHidden "Create Mode"
            , options =
                [ Input.option EC.New (text "New")
                , Input.option EC.Load (text "Load")
                ]
            }
        , case state.createMode of
            EC.New ->
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

            EC.Load ->
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
                EC.New ->
                    Just <| FinishedEdit <| C.Module state.label state.controller

                EC.Load ->
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


editIsomorphicPane : EC.EditIsomorphicState -> Element Msg
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
                (EC.toIsomorphicInput state)
            )
        ]


editScalePane : EC.EditScaleState -> Element Msg
editScalePane state =
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
                    |> EditScale
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
                    |> EditScale
                    |> UpdateControllerState
            )
        , noteRadio
            state.key
            (\newNote ->
                { state | key = newNote }
                    |> EditScale
                    |> UpdateControllerState
            )
        , scaleRadio
            state.scale
            (\newScale ->
                { state | scale = newScale }
                    |> EditScale
                    |> UpdateControllerState
            )
        , editTextBox
            { placeholder = "octave#"
            , label = "Octave Number"
            , current = state.octave
            }
            "number"
            (\newOctave ->
                { state | octave = newOctave }
                    |> EditScale
                    |> UpdateControllerState
            )
        , editTextBox
            { placeholder = "# of octaves"
            , label = "Range"
            , current = state.range
            }
            "number"
            (\newRange ->
                { state | range = newRange }
                    |> EditScale
                    |> UpdateControllerState
            )
        , acceptOrCloseButtons
            "Ok"
            ClosePopUp
            (Maybe.map
                (\i -> FinishedEdit (scaleKeyboard i))
                (EC.toScaleKeyboardInput state)
            )
        ]


editChordBuilderPane : EC.EditChordBuilderState -> Element Msg
editChordBuilderPane state =
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
                    |> EditChordBuilder
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
                    |> EditChordBuilder
                    |> UpdateControllerState
            )
        , noteRadio
            state.key
            (\newNote ->
                { state | key = newNote }
                    |> EditChordBuilder
                    |> UpdateControllerState
            )
        , scaleRadio
            state.scale
            (\newScale ->
                { state | scale = newScale }
                    |> EditChordBuilder
                    |> UpdateControllerState
            )
        , chordTypeRadio
            state.chordType
            (\newChordType ->
                { state | chordType = newChordType }
                    |> EditChordBuilder
                    |> UpdateControllerState
            )
        , editTextBox
            { placeholder = "octave#"
            , label = "Octave Number"
            , current = state.octave
            }
            "number"
            (\newOctave ->
                { state | octave = newOctave }
                    |> EditChordBuilder
                    |> UpdateControllerState
            )
        , editTextBox
            { placeholder = "# of octaves"
            , label = "Range"
            , current = state.range
            }
            "number"
            (\newRange ->
                { state | range = newRange }
                    |> EditChordBuilder
                    |> UpdateControllerState
            )
        , acceptOrCloseButtons
            "Ok"
            ClosePopUp
            (Maybe.map
                (\i -> FinishedEdit (chordKeyboard i))
                (EC.toChordBuildInput state)
            )
        ]


noteRadio : EC.Key -> (EC.Key -> msg) -> Element msg
noteRadio note msg =
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
        , selected = Just note
        , label =
            Input.labelAbove
                [ paddingEach { top = 0, bottom = 10, left = 0, right = 0 }
                ]
                (text "Note")
        , options =
            [ Input.option EC.C (text "C")
            , Input.option EC.Cs (text "C#")
            , Input.option EC.D (text "D")
            , Input.option EC.Ds (text "D#")
            , Input.option EC.Db (text "Db")
            , Input.option EC.E (text "E")
            , Input.option EC.Eb (text "Eb")
            , Input.option EC.F (text "F")
            , Input.option EC.Fs (text "F#")
            , Input.option EC.G (text "G")
            , Input.option EC.Gs (text "G#")
            , Input.option EC.Gb (text "Gb")
            , Input.option EC.A (text "A")
            , Input.option EC.As (text "A#")
            , Input.option EC.Ab (text "Ab")
            , Input.option EC.B (text "B")
            , Input.option EC.Bb (text "Bb")
            ]
        }


scaleRadio : EC.Scale -> (EC.Scale -> msg) -> Element msg
scaleRadio scale msg =
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
        , selected = Just scale
        , label =
            Input.labelAbove
                [ paddingEach { top = 0, bottom = 10, left = 0, right = 0 }
                ]
                (text "Scale")
        , options =
            [ Input.option EC.Major (text "Major")
            , Input.option EC.NaturalMinor (text "Natural Minor")
            , Input.option EC.Ionian (text "Ionian")
            , Input.option EC.Dorian (text "Dorian")
            , Input.option EC.Phrygian (text "Phrygian")
            , Input.option EC.Lydian (text "Lydian")
            , Input.option EC.Mixolydian (text "Mixolydian")
            , Input.option EC.Aeolian (text "Aeolian")
            , Input.option EC.Locrian (text "Locrian")
            , Input.option EC.MelodicMinor (text "Melodic Minor")
            , Input.option EC.DorianFlat2 (text "Dorian Flat2")
            , Input.option EC.LydianAugmented (text "Lydian Augmented")
            , Input.option EC.Acoustic (text "Acoustic")
            , Input.option EC.MajorMinor (text "Major Minor")
            , Input.option EC.MinorLocrian (text "Minor Locrian")
            , Input.option EC.SuperLocrian (text "Superlocrian")
            , Input.option EC.HarmonicMinor (text "Harmonic Minor")
            , Input.option EC.LocrianNatural6 (text "Locrian Natural6")
            , Input.option EC.MajorAugmented (text "Major Augmented")
            , Input.option EC.LydianDiminished (text "Lydian Diminished")
            , Input.option EC.PhrygianDominant (text "Phrygian Dominant")
            , Input.option EC.AeolianHarmonic (text "Aeolian Harmonic")
            , Input.option EC.UltraLocrian (text "Ultralocrian")
            , Input.option EC.DiminishedWholeToneHalfTone (text "Diminished Whole Half Tone")
            , Input.option EC.DiminishedHalfToneWholeTone (text "Diminished Half Whole Tone")
            , Input.option EC.WholeTone (text "Whole Tone")
            , Input.option EC.MajorPentatonic (text "Major Pentatonic")
            , Input.option EC.MinorPentatonic (text "Minor Pentatonic")
            ]
        }


chordTypeRadio : EC.ChordType -> (EC.ChordType -> msg) -> Element msg
chordTypeRadio chordType msg =
    Input.radioRow
        [ padding 4
        , spacing 10
        , height (px 36)
        , width fill
        , scrollbarY
        , Border.width 2
        , Border.dashed
        ]
        { onChange = msg
        , selected = Just chordType
        , label =
            Input.labelAbove
                [ paddingEach { top = 0, bottom = 10, left = 0, right = 0 }
                ]
                (text "Chord Type")
        , options =
            [ Input.option EC.Triad (text "Triad")
            , Input.option EC.FourNote (text "Four Note")
            ]
        }


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


editSizedRowPane : List ( Int, Controller ) -> Element Msg
editSizedRowPane subControls =
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
                    List.append subControls [ ( 1, C.Space ) ]
                        |> EditSizedRow
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
                        |> EditSizedRow
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
            (List.indexedMap
                (\i ( s, c ) ->
                    row [ width fill ]
                        [ el [] (C.controllerToString c |> text)
                        , row [ alignRight, spacing 2 ]
                            [ Input.button
                                [ padding 5
                                , Border.width 2
                                , Border.solid
                                , borderColour Black
                                ]
                                { onPress =
                                    List.indexedMap
                                        (\ci ( cs, cc ) ->
                                            if ci == i then
                                                ( max (cs - 1) 0, cc )

                                            else
                                                ( cs, cc )
                                        )
                                        subControls
                                        |> EditSizedRow
                                        |> UpdateControllerState
                                        |> Just
                                , label = text "-"
                                }
                            , el [ alignRight ] (String.fromInt s |> text)
                            , Input.button
                                [ padding 5
                                , Border.width 2
                                , Border.solid
                                , borderColour Black
                                ]
                                { onPress =
                                    List.indexedMap
                                        (\ci ( cs, cc ) ->
                                            if ci == i then
                                                ( cs + 1, cc )

                                            else
                                                ( cs, cc )
                                        )
                                        subControls
                                        |> EditSizedRow
                                        |> UpdateControllerState
                                        |> Just
                                , label = text "+"
                                }
                            ]
                        ]
                )
                subControls
            )
        , acceptOrCloseButtons
            "Ok"
            ClosePopUp
            (Just <| FinishedEdit <| C.SizedRow subControls)
        ]


editSizedColumnPane : List ( Int, Controller ) -> Element Msg
editSizedColumnPane subControls =
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
                    List.append subControls [ ( 1, C.Space ) ]
                        |> EditSizedColumn
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
                        |> EditSizedColumn
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
            (List.indexedMap
                (\i ( s, c ) ->
                    row [ width fill ]
                        [ el [] (C.controllerToString c |> text)
                        , row [ alignRight, spacing 2 ]
                            [ Input.button
                                [ padding 5
                                , Border.width 2
                                , Border.solid
                                , borderColour Black
                                ]
                                { onPress =
                                    List.indexedMap
                                        (\ci ( cs, cc ) ->
                                            if ci == i then
                                                ( max (cs - 1) 0, cc )

                                            else
                                                ( cs, cc )
                                        )
                                        subControls
                                        |> EditSizedColumn
                                        |> UpdateControllerState
                                        |> Just
                                , label = text "-"
                                }
                            , el [ alignRight ] (String.fromInt s |> text)
                            , Input.button
                                [ padding 5
                                , Border.width 2
                                , Border.solid
                                , borderColour Black
                                ]
                                { onPress =
                                    List.indexedMap
                                        (\ci ( cs, cc ) ->
                                            if ci == i then
                                                ( cs + 1, cc )

                                            else
                                                ( cs, cc )
                                        )
                                        subControls
                                        |> EditSizedColumn
                                        |> UpdateControllerState
                                        |> Just
                                , label = text "+"
                                }
                            ]
                        ]
                )
                subControls
            )
        , acceptOrCloseButtons
            "Ok"
            ClosePopUp
            (Just <| FinishedEdit <| C.SizedColumn subControls)
        ]


editNotePane : EC.EditNoteState -> Element Msg
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
                (EC.editStateToNote state)
            )
        ]


editChordPane : EC.EditChordState -> Element Msg
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
                (EC.editStateToChord state)
            )
        ]


editCCValuePane : EC.EditCCValueState -> Element Msg
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
                (EC.editStateToCCValue state)
            )
        ]


editCommandPane : EC.EditCommandState -> Element Msg
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
                [ Input.option EC.OnPressMsgs (text "Button Press")
                , Input.option EC.OnReleaseMsgs (text "Button Release")
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
                                            EC.OnPressMsgs ->
                                                { state | onPressMsgs = [] }

                                            EC.OnReleaseMsgs ->
                                                { state | onReleaseMsgs = [] }
                                        )
                                            |> EditCommand
                                            |> UpdateControllerState
                                            |> Just
                                    , label = text "Clear"
                                    }
                                ]
                                :: ((case state.editMode of
                                        EC.OnPressMsgs ->
                                            state.onPressMsgs

                                        EC.OnReleaseMsgs ->
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
                (EC.editStateToCommand state)
            )
        ]


newCommandMidiMsgView : Midi.EditMidiButtonMsg -> EC.EditCommandState -> Element Msg
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
                                EC.OnPressMsgs ->
                                    { state
                                        | onPressMsgs =
                                            List.append state.onPressMsgs [ completeMsg ]
                                        , newMsg = Nothing
                                    }
                                        |> EditCommand
                                        |> UpdateControllerState

                                EC.OnReleaseMsgs ->
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


editSequencePane : EC.EditSequenceState -> Element Msg
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
                (EC.editStateToSequence state)
            )
        ]


newSequenceMidiMsgView : Midi.EditMidiButtonMsg -> EC.EditSequenceState -> Element Msg
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


editFaderPane : EC.EditFaderState -> Element Msg
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
                (EC.editStateToFader state)
            )
        ]


editXYFaderPane : EC.EditXYFaderState -> Element Msg
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
                [ Input.option EC.Params1 (text "Params X")
                , Input.option EC.Params2 (text "Params Y")
                ]
            }
        , case state.active of
            EC.Params1 ->
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

            EC.Params2 ->
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
                (EC.editStateToXYFader state)
            )
        ]


editPitchBendPane : EC.EditPitchBendState -> Element Msg
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
                (EC.editStateToPitchBend state)
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
        C.renderController
            { addSpaceMsg = AddSpace
            , buttonDownMsg = ButtonDown
            , buttonUpMsg = ButtonUp
            , editMsg = OpenEditController
            , faderChangingMsg = FaderChanging
            , faderChangingMouseMsg = FaderChangingMouse
            , faderSetMsg = FaderSet
            , noopMsg = NoOp
            , removeItemMsg = RemoveItem
            }
            (case mode of
                Normal ->
                    False

                Edit _ ->
                    True
            )
            midiLog
            config
            []
            controller
            0



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
        [ Ports.webMidiNotAvailable WebMidiNotAvailable
        , Ports.midiDevices MidiDevicesChanged
        , Ports.incomingMidi IncomingMidi
        , Browser.Events.onResize PageResized
        ]



-- }}}
