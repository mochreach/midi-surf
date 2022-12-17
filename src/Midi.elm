module Midi exposing
    ( Device
    , Status(..)
    , statusView
    )

import Element exposing (..)


type Status
    = Initialising
    | FailedToEstablishMidi
    | MidiAvailable (List Device)


statusView : Status -> Element msg
statusView status =
    case status of
        Initialising ->
            paragraph [] [ text "Initialising..." ]

        FailedToEstablishMidi ->
            paragraph [] [ text "Failed to establish MIDI connection." ]

        MidiAvailable devices ->
            paragraph [] [ text <| "MIDI connection: " ++ String.fromInt (List.length devices) ]


type alias Device =
    { name : String
    , input : Maybe Bool
    , output : Maybe Bool
    }
