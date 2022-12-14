port module Ports exposing
    ( connectToDevice
    , connectedToDevice
    , listenForMIDIStatus
    , sendNoteOff
    , sendNoteOn
    )


port listenForMIDIStatus : (List ( String, String ) -> msg) -> Sub msg


port connectedToDevice : (String -> msg) -> Sub msg


port connectToDevice : String -> Cmd msg


port sendNoteOn : { noteNumber : Int, channel : Int } -> Cmd msg


port sendNoteOff : { noteNumber : Int, channel : Int } -> Cmd msg
