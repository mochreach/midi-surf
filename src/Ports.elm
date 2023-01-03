port module Ports exposing
    ( incomingMidi
    , midiDevices
    , midiMsgToCmd
    , saveState
    , sendCC
    , sendNoteOff
    , sendNoteOn
    )

import Array exposing (Array)
import Codec exposing (Value)
import Midi exposing (MidiMsg(..))


port midiDevices : (List Midi.Device -> msg) -> Sub msg


port incomingMidi : ({ deviceName : String, midiData : Array Int } -> msg) -> Sub msg


port sendNoteOn : { pitch : Int, velocity : Int, channel : Int } -> Cmd msg


port sendNoteOff : { pitch : Int, velocity : Int, channel : Int } -> Cmd msg


port sendCC : { channel : Int, controller : Int, value : Int } -> Cmd msg


port saveState : Value -> Cmd msg


midiMsgToCmd : MidiMsg -> Cmd msg
midiMsgToCmd midiMsg =
    case midiMsg of
        NoteOn { channel, pitch, velocity } ->
            sendNoteOn
                { channel = channel
                , pitch = pitch
                , velocity = velocity
                }

        NoteOff { channel, pitch, velocity } ->
            sendNoteOff
                { channel = channel
                , pitch = pitch
                , velocity = velocity
                }

        Midi.ControllerChange data ->
            sendCC data

        _ ->
            Cmd.none
