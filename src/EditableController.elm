module EditableController exposing (..)

import Controller exposing (Controller)
import Midi exposing (MidiMsg(..))
import Style exposing (..)


type EditableController
    = EditModule String Controller
    | EditColumn (List Controller)
    | EditRow (List Controller)
    | EditNote EditNoteState
    | EditCCValue EditCCValueState
    | EditFader EditFaderState
    | EditSpace


type alias EditNoteState =
    { label : String
    , colour : AppColour
    , channel : String
    , noteNumber : String
    , velocity : String
    }


defaultEditNoteState : EditNoteState
defaultEditNoteState =
    { label = ""
    , colour = LightGrey
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
editStateToNote { label, colour, noteNumber, channel, velocity } =
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
                Controller.newNote label colour ch nn vel
                    |> Just

            _ ->
                Nothing


type alias EditCCValueState =
    { label : String
    , colour : AppColour
    , channel : String
    , controller : String
    , value : String
    }


defaultEditCCValueState : EditCCValueState
defaultEditCCValueState =
    { label = ""
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


editStateToCCValue : EditCCValueState -> Maybe Controller
editStateToCCValue { label, colour, channel, controller, value } =
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
                Controller.newCCValue label colour ch c v
                    |> Just

            _ ->
                Nothing


type alias EditFaderState =
    { label : String
    , colour : AppColour
    , channel : String
    , ccNumber : String
    , valueMin : String
    , valueMax : String
    }


defaultEditFaderState : EditFaderState
defaultEditFaderState =
    { label = ""
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


editFaderToFader : EditFaderState -> Maybe Controller
editFaderToFader { label, colour, channel, ccNumber, valueMin, valueMax } =
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


updateWithMidiMsg : MidiMsg -> EditableController -> EditableController
updateWithMidiMsg midiMsg state =
    case state of
        EditModule _ _ ->
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
                    List.append subControls
                        [ Controller.newNote label
                            (pitchToAppColour pitch)
                            ch
                            pitch
                            velocity
                        ]
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
                            Controller.midiNumberToChannel channel
                                |> Maybe.withDefault Controller.Ch1

                        label =
                            "Ch"
                                ++ Controller.channelToString ch
                                ++ "#"
                                ++ String.fromInt pitch
                    in
                    List.append subControls
                        [ Controller.newNote
                            label
                            (pitchToAppColour pitch)
                            ch
                            pitch
                            velocity
                        ]
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

        EditCCValue ccState ->
            updateEditCCValueWithMidiMsg midiMsg ccState
                |> EditCCValue

        EditFader faderState ->
            updateEditFaderWithMidiMsg midiMsg faderState
                |> EditFader

        EditSpace ->
            state
