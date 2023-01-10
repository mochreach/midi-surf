port module Ports exposing (..)

import Array exposing (Array)
import Codec exposing (Value)
import Midi exposing (MidiMsg(..))


port midiDevices : (List Midi.Device -> msg) -> Sub msg


port incomingMidi : ({ deviceName : String, midiData : Array Int } -> msg) -> Sub msg


port outgoingMidi : Array (Array Int) -> Cmd msg


port saveState : Value -> Cmd msg
