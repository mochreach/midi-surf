port module Ports exposing
    ( midiDevices
    , sendNoteOff
    , sendNoteOn
    )

import Midi


port midiDevices : (List Midi.Device -> msg) -> Sub msg


port sendNoteOn : { noteNumber : Int, channel : Int } -> Cmd msg


port sendNoteOff : { noteNumber : Int, channel : Int } -> Cmd msg
