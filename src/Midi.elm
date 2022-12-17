module Midi exposing
    ( MidiConnection
    , MidiStatus(..)
    , midiStatus
    )

import Element exposing (..)


type MidiStatus
    = Initialising
    | FailedToEstablishMidi
    | MidiAvailable (List MidiConnection)


midiStatus : MidiStatus -> Element msg
midiStatus status =
    case status of
        Initialising ->
            paragraph [] [ text "Initialising..." ]

        FailedToEstablishMidi ->
            paragraph [] [ text "Failed to establish MIDI connection." ]

        MidiAvailable devices ->
            paragraph [] [ text <| "MIDI connection: " ++ String.fromInt (List.length devices) ]


type alias MidiConnection =
    { id : String
    , name : String
    , input : Maybe Bool
    , output : Maybe Bool
    }
