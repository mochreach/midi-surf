module Midi exposing
    ( Device
    , MidiMsg(..)
    , Status(..)
    , intArrayToMidiMsg
    , statusView
    )

import Array exposing (Array)
import Element exposing (..)


type MidiMsg
    = NoteOn { channel : Int, pitch : Int, velocity : Int }
    | NoteOff { channel : Int, pitch : Int, velocity : Int }
    | KeyPressure { channel : Int, key : Int, pressure : Int }
    | ControllerChange { channel : Int, controller : Int, value : Int }
    | ProgramChange { channel : Int, preset : Int }
    | ChannelPressure { channel : Int, pressure : Int }
    | PitchBend { channel : Int, bendLSB : Int, bendMSB : Int }
    | SystemExclusive { vendorId : Int, data : Array Int }
    | SongPosition { positionLSB : Int, positionMSB : Int }
    | SongSelect Int
    | UnofficialBusSelect Int
    | TuneRequest
    | EndOfSysEx
    | TimingTick
    | StartSong
    | ContinueSong
    | StopSong
    | ActiveSensing
    | SystemReset
    | Unknown (Array Int)


intArrayToMidiMsg : Array Int -> MidiMsg
intArrayToMidiMsg intArray =
    let
        firstByte =
            Array.get 0 intArray

        msgNibble =
            Maybe.map (\v -> v // 16) firstByte

        channelNibble =
            Maybe.map (\v -> modBy 16 v) firstByte

        secondByte =
            Array.get 1 intArray

        thirdByte =
            Array.get 2 intArray
    in
    case ( firstByte, secondByte, thirdByte ) of
        ( Just 0xF0, Just vendorId, _ ) ->
            SystemExclusive
                { vendorId = vendorId
                , data = intArray
                }

        ( Just 0xF2, Just positionLSB, Just positionMSB ) ->
            SongPosition { positionLSB = positionLSB, positionMSB = positionMSB }

        ( Just 0xF3, Just songNumber, _ ) ->
            SongSelect songNumber

        ( Just 0xF5, Just busNumber, _ ) ->
            UnofficialBusSelect busNumber

        ( Just 0xF6, _, _ ) ->
            TuneRequest

        ( Just 0xF7, _, _ ) ->
            EndOfSysEx

        ( Just 0xF8, _, _ ) ->
            TimingTick

        ( Just 0xFA, _, _ ) ->
            StartSong

        ( Just 0xFB, _, _ ) ->
            ContinueSong

        ( Just 0xFC, _, _ ) ->
            StopSong

        ( Just 0xFE, _, _ ) ->
            ActiveSensing

        ( Just 0xFF, _, _ ) ->
            SystemReset

        ( Just _, Just byte1, _ ) ->
            case ( msgNibble, channelNibble, thirdByte ) of
                ( Just 8, Just channel, Just velocity ) ->
                    NoteOff { channel = channel, pitch = byte1, velocity = velocity }

                ( Just 9, Just channel, Just velocity ) ->
                    NoteOn { channel = channel, pitch = byte1, velocity = velocity }

                ( Just 10, Just channel, Just pressure ) ->
                    KeyPressure { channel = channel, key = byte1, pressure = pressure }

                ( Just 11, Just channel, Just value ) ->
                    ControllerChange { channel = channel, controller = byte1, value = value }

                ( Just 12, Just channel, _ ) ->
                    ProgramChange { channel = channel, preset = byte1 }

                ( Just 13, Just channel, _ ) ->
                    ChannelPressure { channel = channel, pressure = byte1 }

                ( Just 14, Just channel, Just bendMSB ) ->
                    PitchBend { channel = channel, bendLSB = byte1, bendMSB = bendMSB }

                _ ->
                    Unknown intArray

        _ ->
            Unknown intArray


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
