port module Ports exposing (listenForMIDIStatus)


port listenForMIDIStatus : (Bool -> msg) -> Sub msg
