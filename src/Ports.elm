port module Ports exposing
    ( incomingMidi
    , midiDevices
    , sendNoteOff
    , sendNoteOn
    )

import Array exposing (Array)
import Midi


port midiDevices : (List Midi.Device -> msg) -> Sub msg


port incomingMidi : (Array Int -> msg) -> Sub msg


port sendNoteOn : { noteNumber : Int, channel : Int } -> Cmd msg


port sendNoteOff : { noteNumber : Int, channel : Int } -> Cmd msg
