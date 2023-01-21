module EditableController exposing (..)

import Controller exposing (Controller)
import Dict exposing (Dict)
import Midi exposing (MidiMsg(..))
import Style exposing (..)
import Utils


type EditableController
    = EditModule EditModuleState
    | EditIsomorphic EditIsomorphicState
    | EditColumn (List Controller)
    | EditRow (List Controller)
    | EditNote EditNoteState
    | EditChord EditChordState
    | EditCCValue EditCCValueState
    | EditCommand EditCommandState
    | EditFader EditFaderState
    | EditXYFader EditXYFaderState
    | EditMidiLog
    | EditSpace


type alias EditModuleState =
    { label : String
    , controller : Controller
    , createMode : CreateMode
    , selectedModule : Maybe Int
    }


type CreateMode
    = New
    | Load


type alias EditIsomorphicState =
    { channel : String
    , velocity : String
    , firstNote : String
    , numberOfRows : String
    , offset : String
    , rowLength : String
    }


defaultEditIsomorphicState : EditIsomorphicState
defaultEditIsomorphicState =
    { channel = ""
    , velocity = ""
    , firstNote = ""
    , numberOfRows = ""
    , offset = ""
    , rowLength = ""
    }


toIsomorphicInput :
    EditIsomorphicState
    ->
        Maybe
            { channel : Midi.Channel
            , velocity : Int
            , firstNote : Int
            , numberOfRows : Int
            , offset : Int
            , rowLength : Int
            }
toIsomorphicInput state =
    let
        mChannel =
            Midi.stringToChannel state.channel

        mVelocity =
            String.toInt state.velocity

        mFirstNote =
            String.toInt state.firstNote

        mNumberOfRows =
            String.toInt state.numberOfRows

        mOffset =
            String.toInt state.offset

        mRowLength =
            String.toInt state.rowLength
    in
    Utils.mmap6
        (\c v f n o r ->
            { channel = c
            , velocity = v
            , firstNote = f
            , numberOfRows = n
            , offset = o
            , rowLength = r
            }
        )
        mChannel
        mVelocity
        mFirstNote
        mNumberOfRows
        mOffset
        mRowLength


type alias EditNoteState =
    { label : String
    , labelSize : LabelSize
    , colour : AppColour
    , channel : String
    , pitch : String
    , velocity : String
    }


defaultEditNoteState : EditNoteState
defaultEditNoteState =
    { label = ""
    , labelSize = Medium
    , colour = LightGrey
    , channel = "1"
    , pitch = "60"
    , velocity = "100"
    }


updateEditNoteWithMidiMsg : MidiMsg -> EditNoteState -> EditNoteState
updateEditNoteWithMidiMsg midiMsg state =
    case midiMsg of
        NoteOn noteOnParams ->
            { state
              -- Adding 1 to the channel so that they're labelled 1-16
                | channel = String.fromInt (noteOnParams.channel + 1)
                , pitch = String.fromInt noteOnParams.pitch
                , velocity = String.fromInt noteOnParams.velocity
            }

        _ ->
            state


editStateToNote : EditNoteState -> Maybe Controller
editStateToNote { label, labelSize, colour, pitch, channel, velocity } =
    case
        ( Midi.stringToChannel channel
        , String.toInt pitch
        , String.toInt velocity
        )
    of
        ( Just ch, Just nn, Just vel ) ->
            -- TODO: These values should not exceed 127, handle with midi module
            Controller.newNote label labelSize colour ch nn vel
                |> Just

        _ ->
            Nothing


type alias EditChordState =
    { label : String
    , labelSize : LabelSize
    , colour : AppColour
    , velocity : String
    , notes :
        Dict
            ( Int, Int )
            { channel : Midi.Channel
            , pitch : Int
            }
    }


defaultEditChordState : EditChordState
defaultEditChordState =
    { label = ""
    , labelSize = Medium
    , colour = LightGrey
    , velocity = "100"
    , notes = Dict.empty
    }


updateEditChordWithMidiMsg : MidiMsg -> EditChordState -> EditChordState
updateEditChordWithMidiMsg midiMsg state =
    case midiMsg of
        NoteOn noteOnParams ->
            { state
              -- Adding 1 to the channel so that they're labelled 1-16
                | notes =
                    Dict.insert
                        ( Midi.intToChannel noteOnParams.channel
                            |> Maybe.withDefault Midi.Ch1
                            |> Midi.channelToMidiNumber
                        , noteOnParams.pitch
                        )
                        { channel =
                            Midi.intToChannel noteOnParams.channel
                                |> Maybe.withDefault Midi.Ch1
                        , pitch = noteOnParams.pitch
                        }
                        state.notes
            }

        _ ->
            state


editStateToChord : EditChordState -> Maybe Controller
editStateToChord { label, labelSize, colour, velocity, notes } =
    String.toInt velocity
        |> Maybe.map
            (\v -> Controller.newChord label labelSize colour v (Dict.values notes))


type alias EditCCValueState =
    { label : String
    , labelSize : LabelSize
    , colour : AppColour
    , channel : String
    , controller : String
    , value : String
    }


defaultEditCCValueState : EditCCValueState
defaultEditCCValueState =
    { label = ""
    , labelSize = Medium
    , colour = Green
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


editStateToCommand : EditCommandState -> Maybe Controller
editStateToCommand { label, labelSize, colour, onPressMsgs, onReleaseMsgs } =
    Controller.newCommand label labelSize colour onPressMsgs onReleaseMsgs
        |> Just


type alias EditCommandState =
    { label : String
    , labelSize : LabelSize
    , colour : AppColour
    , editMode : CommandEditMode
    , onPressMsgs : List MidiMsg
    , onReleaseMsgs : List MidiMsg
    , newMsg : Maybe Midi.EditMidiButtonMsg
    }


type CommandEditMode
    = OnPressMsgs
    | OnReleaseMsgs


defaultEditCommandState : EditCommandState
defaultEditCommandState =
    { label = ""
    , labelSize = Medium
    , colour = Green
    , editMode = OnPressMsgs
    , onPressMsgs = []
    , onReleaseMsgs = []
    , newMsg = Nothing
    }


updateEditCommandWithMidiMsg : MidiMsg -> EditCommandState -> EditCommandState
updateEditCommandWithMidiMsg midiMsg state =
    { state | newMsg = Midi.midiMsgToEditMidiButtonMsg midiMsg }


editStateToCCValue : EditCCValueState -> Maybe Controller
editStateToCCValue { label, labelSize, colour, channel, controller, value } =
    case
        ( Midi.stringToChannel channel
        , String.toInt controller
        , String.toInt value
        )
    of
        ( Just ch, Just c, Just v ) ->
            -- TODO: These values should not exceed 127, handle with midi module
            Controller.newCCValue label labelSize colour ch c v
                |> Just

        _ ->
            Nothing


type alias EditFaderState =
    { label : String
    , labelSize : LabelSize
    , colour : AppColour
    , channel : String
    , ccNumber : String
    , valueMin : String
    , valueMax : String
    }


defaultEditFaderState : EditFaderState
defaultEditFaderState =
    { label = ""
    , labelSize = Medium
    , colour = Yellow
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


editStateToFader : EditFaderState -> Maybe Controller
editStateToFader { label, labelSize, colour, channel, ccNumber, valueMin, valueMax } =
    case Midi.stringToChannel channel of
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
                        , labelSize = labelSize
                        , colour = colour
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


type alias EditXYFaderState =
    { label : String
    , labelSize : LabelSize
    , colour : AppColour
    , active : XYParamsActive
    , channel1 : String
    , ccNumber1 : String
    , valueMin1 : String
    , valueMax1 : String
    , channel2 : String
    , ccNumber2 : String
    , valueMin2 : String
    , valueMax2 : String
    }


type XYParamsActive
    = Params1
    | Params2


defaultEditXYFaderState : EditXYFaderState
defaultEditXYFaderState =
    { label = ""
    , labelSize = Medium
    , colour = Yellow
    , active = Params1
    , channel1 = "1"
    , ccNumber1 = "1"
    , valueMin1 = "0"
    , valueMax1 = "127"
    , channel2 = "1"
    , ccNumber2 = "2"
    , valueMin2 = "0"
    , valueMax2 = "127"
    }


updateEditXYFaderWithMidiMsg : MidiMsg -> EditXYFaderState -> EditXYFaderState
updateEditXYFaderWithMidiMsg midiMsg state =
    case midiMsg of
        ControllerChange { channel, controller } ->
            case state.active of
                Params1 ->
                    { state
                      -- Adding 1 to the channel so that they're labelled 1-16
                        | channel1 = String.fromInt (channel + 1)
                        , ccNumber1 = String.fromInt controller
                    }

                Params2 ->
                    { state
                      -- Adding 1 to the channel so that they're labelled 1-16
                        | channel2 = String.fromInt (channel + 1)
                        , ccNumber2 = String.fromInt controller
                    }

        _ ->
            state


editStateToXYFader : EditXYFaderState -> Maybe Controller
editStateToXYFader state =
    case ( Midi.stringToChannel state.channel1, Midi.stringToChannel state.channel2 ) of
        ( Just ch1, Just ch2 ) ->
            case
                [ String.toInt state.ccNumber1
                , String.toInt state.valueMin1
                , String.toInt state.valueMax1
                , String.toInt state.ccNumber2
                , String.toInt state.valueMin2
                , String.toInt state.valueMax2
                ]
            of
                [ Just cc1, Just vmin1, Just vmax1, Just cc2, Just vmin2, Just vmax2 ] ->
                    Controller.XYFader
                        { status = Controller.Set
                        , label = state.label
                        , labelSize = state.labelSize
                        , colour = state.colour
                        , channel1 = ch1
                        , ccNumber1 = cc1
                        , valuePercent1 = 50
                        , valueMin1 = vmin1
                        , valueMax1 = vmax1
                        , channel2 = ch2
                        , ccNumber2 = cc2
                        , valuePercent2 = 50
                        , valueMin2 = vmin2
                        , valueMax2 = vmax2
                        }
                        |> Just

                _ ->
                    Nothing

        _ ->
            Nothing


updateWithMidiMsg : MidiMsg -> EditableController -> EditableController
updateWithMidiMsg midiMsg state =
    case state of
        EditModule _ ->
            state

        EditIsomorphic _ ->
            state

        EditColumn subControls ->
            case midiMsg of
                Midi.NoteOn { channel, pitch, velocity } ->
                    let
                        ch =
                            Midi.intToChannel channel
                                |> Maybe.withDefault Midi.Ch1

                        label =
                            "Ch"
                                ++ Midi.channelToString ch
                                ++ "#"
                                ++ String.fromInt pitch
                    in
                    List.append subControls
                        [ Controller.newNote
                            label
                            Medium
                            (pitchToAppColour pitch)
                            ch
                            pitch
                            velocity
                        ]
                        |> EditColumn

                Midi.ControllerChange { channel, controller } ->
                    let
                        ch =
                            Midi.intToChannel channel
                                |> Maybe.withDefault Midi.Ch1

                        label =
                            "Ch"
                                ++ Midi.channelToString ch
                                ++ " CC "
                                ++ String.fromInt controller
                    in
                    List.append
                        subControls
                        [ Controller.Fader
                            { status = Controller.Set
                            , label = label
                            , labelSize = Medium
                            , colour = Yellow
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
                            Midi.intToChannel channel
                                |> Maybe.withDefault Midi.Ch1

                        label =
                            "Ch"
                                ++ Midi.channelToString ch
                                ++ "#"
                                ++ String.fromInt pitch
                    in
                    List.append subControls
                        [ Controller.newNote
                            label
                            Medium
                            (pitchToAppColour pitch)
                            ch
                            pitch
                            velocity
                        ]
                        |> EditRow

                Midi.ControllerChange { channel, controller } ->
                    let
                        ch =
                            Midi.intToChannel channel
                                |> Maybe.withDefault Midi.Ch1

                        label =
                            "Ch"
                                ++ Midi.channelToString ch
                                ++ " CC "
                                ++ String.fromInt controller
                    in
                    List.append
                        subControls
                        [ Controller.Fader
                            { status = Controller.Set
                            , label = label
                            , labelSize = Medium
                            , colour = Yellow
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

        EditChord chordState ->
            updateEditChordWithMidiMsg midiMsg chordState
                |> EditChord

        EditCCValue ccState ->
            updateEditCCValueWithMidiMsg midiMsg ccState
                |> EditCCValue

        EditCommand comState ->
            updateEditCommandWithMidiMsg midiMsg comState
                |> EditCommand

        EditFader faderState ->
            updateEditFaderWithMidiMsg midiMsg faderState
                |> EditFader

        EditXYFader xyFaderState ->
            updateEditXYFaderWithMidiMsg midiMsg xyFaderState
                |> EditXYFader

        EditMidiLog ->
            state

        EditSpace ->
            state
