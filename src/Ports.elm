port module Ports exposing
    ( incomingMidi
    , midiDevices
    , sendCC
    , sendNoteOff
    , sendNoteOn
    )

import Array exposing (Array)
import Midi


port midiDevices : (List Midi.Device -> msg) -> Sub msg


port incomingMidi : (Array Int -> msg) -> Sub msg


port sendNoteOn : { pitch : Int, velocity : Int, channel : Int } -> Cmd msg


port sendNoteOff : { pitch : Int, velocity : Int, channel : Int } -> Cmd msg


port sendCC : { channel : Int, controller : Int, value : Int } -> Cmd msg
