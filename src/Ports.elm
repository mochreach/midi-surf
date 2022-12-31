port module Ports exposing
    ( incomingMidi
    , midiDevices
    , saveState
    , sendCC
    , sendNoteOff
    , sendNoteOn
    )

import Array exposing (Array)
import Codec exposing (Value)
import Midi


port midiDevices : (List Midi.Device -> msg) -> Sub msg


port incomingMidi : ({ deviceName : String, midiData : Array Int } -> msg) -> Sub msg


port sendNoteOn : { pitch : Int, velocity : Int, channel : Int } -> Cmd msg


port sendNoteOff : { pitch : Int, velocity : Int, channel : Int } -> Cmd msg


port sendCC : { channel : Int, controller : Int, value : Int } -> Cmd msg


port saveState : Value -> Cmd msg
