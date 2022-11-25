port module Ports exposing (listenForMIDIStatus, sendNoteOff, sendNoteOn)


port listenForMIDIStatus : (Bool -> msg) -> Sub msg


port sendNoteOn : Int -> Cmd msg


port sendNoteOff : Int -> Cmd msg
