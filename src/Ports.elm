port module Ports exposing
    ( midiStatus
    , sendNoteOff
    , sendNoteOn
    )

import Midi


port midiStatus : (List Midi.MidiConnection -> msg) -> Sub msg


port sendNoteOn : { noteNumber : Int, channel : Int } -> Cmd msg


port sendNoteOff : { noteNumber : Int, channel : Int } -> Cmd msg
