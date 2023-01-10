module Midi exposing (..)

import Array exposing (Array)
import Codec exposing (Codec)
import Element exposing (..)



-- {{{ MidiMsg


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


midiMsgToString : MidiMsg -> String
midiMsgToString midiMsg =
    case midiMsg of
        NoteOn { channel, pitch, velocity } ->
            "Note On: Channel "
                ++ String.fromInt channel
                ++ ", Pitch "
                ++ String.fromInt pitch
                ++ ", Velocity "
                ++ String.fromInt velocity

        NoteOff { channel, pitch, velocity } ->
            "Note Off: Channel "
                ++ String.fromInt channel
                ++ ", Pitch "
                ++ String.fromInt pitch
                ++ ", Velocity "
                ++ String.fromInt velocity

        KeyPressure { channel, key, pressure } ->
            "Key Pressure: Channel "
                ++ String.fromInt channel
                ++ ", Key "
                ++ String.fromInt key
                ++ ", Pressure "
                ++ String.fromInt pressure

        ControllerChange { channel, controller, value } ->
            "Controller Change: Channel "
                ++ String.fromInt channel
                ++ ", Controller "
                ++ String.fromInt controller
                ++ ", Value "
                ++ String.fromInt value

        ProgramChange { channel, preset } ->
            "Program Change: Channel "
                ++ String.fromInt channel
                ++ ", Preset "
                ++ String.fromInt preset

        ChannelPressure { channel, pressure } ->
            "Channel Pressure: Channel "
                ++ String.fromInt channel
                ++ ", Pressure "
                ++ String.fromInt pressure

        PitchBend { channel, bendLSB, bendMSB } ->
            "Pitch Bend: Channel "
                ++ String.fromInt channel
                ++ ", LSB "
                ++ String.fromInt bendLSB
                ++ ", MSB "
                ++ String.fromInt bendMSB

        SystemExclusive { vendorId, data } ->
            "System Exclusive: Vendor ID "
                ++ String.fromInt vendorId
                ++ ", Data "
                ++ (Array.map String.fromInt data |> Array.toList |> String.join " ")

        SongPosition { positionLSB, positionMSB } ->
            "Song Position:  LSB "
                ++ String.fromInt positionLSB
                ++ ", MSB "
                ++ String.fromInt positionMSB

        SongSelect songNumber ->
            "Song Select:  Song Number "
                ++ String.fromInt songNumber

        UnofficialBusSelect busNumber ->
            "Unofficial Bus Select: Bus Number "
                ++ String.fromInt busNumber

        TuneRequest ->
            "Tune Request"

        EndOfSysEx ->
            "End Of Sys Ex"

        TimingTick ->
            "Timing Tick"

        StartSong ->
            "Start Song"

        ContinueSong ->
            "Continue Song"

        StopSong ->
            "Stop Song"

        ActiveSensing ->
            "Active Sensing"

        SystemReset ->
            "System Reset"

        Unknown data ->
            "Unknown MIDI Msg: "
                ++ (Array.map String.fromInt data |> Array.toList |> String.join " ")


midiMsgToIntArray : MidiMsg -> Array Int
midiMsgToIntArray midiMsg =
    case midiMsg of
        NoteOff { channel, pitch, velocity } ->
            Array.fromList [ 0x80 + channel, pitch, velocity ]

        NoteOn { channel, pitch, velocity } ->
            Array.fromList [ 0x90 + channel, pitch, velocity ]

        KeyPressure { channel, key, pressure } ->
            Array.fromList [ 0xA0 + channel, key, pressure ]

        ControllerChange { channel, controller, value } ->
            Array.fromList [ 0xB0 + channel, controller, value ]

        ProgramChange { channel, preset } ->
            Array.fromList [ 0xC0 + channel, preset ]

        ChannelPressure { channel, pressure } ->
            Array.fromList [ 0xD0 + channel, pressure ]

        PitchBend { channel, bendLSB, bendMSB } ->
            Array.fromList [ 0xE0 + channel, bendLSB, bendMSB ]

        SystemExclusive { vendorId, data } ->
            Array.fromList [ 0xF0, vendorId ]
                |> Array.append data

        SongPosition { positionLSB, positionMSB } ->
            Array.fromList [ 0xF2, positionLSB, positionMSB ]

        SongSelect songNumber ->
            Array.fromList [ 0xF3, songNumber ]

        UnofficialBusSelect busNumber ->
            Array.fromList [ 0xF5, busNumber ]

        TuneRequest ->
            Array.fromList [ 0xF6 ]

        EndOfSysEx ->
            Array.fromList [ 0xF7 ]

        TimingTick ->
            Array.fromList [ 0xF8 ]

        StartSong ->
            Array.fromList [ 0xFA ]

        ContinueSong ->
            Array.fromList [ 0xFB ]

        StopSong ->
            Array.fromList [ 0xFC ]

        ActiveSensing ->
            Array.fromList [ 0xFE ]

        SystemReset ->
            Array.fromList [ 0xFF ]

        Unknown data ->
            data


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



-- }}}
-- {{{ Status


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



-- }}}
-- {{{ Channel


type Channel
    = Ch1
    | Ch2
    | Ch3
    | Ch4
    | Ch5
    | Ch6
    | Ch7
    | Ch8
    | Ch9
    | Ch10
    | Ch11
    | Ch12
    | Ch13
    | Ch14
    | Ch15
    | Ch16


channelCodec : Codec Channel
channelCodec =
    Codec.custom
        (\c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 value ->
            case value of
                Ch1 ->
                    c1

                Ch2 ->
                    c2

                Ch3 ->
                    c3

                Ch4 ->
                    c4

                Ch5 ->
                    c5

                Ch6 ->
                    c6

                Ch7 ->
                    c7

                Ch8 ->
                    c8

                Ch9 ->
                    c9

                Ch10 ->
                    c10

                Ch11 ->
                    c11

                Ch12 ->
                    c12

                Ch13 ->
                    c13

                Ch14 ->
                    c14

                Ch15 ->
                    c15

                Ch16 ->
                    c16
        )
        |> Codec.variant0 "Ch1" Ch1
        |> Codec.variant0 "Ch2" Ch2
        |> Codec.variant0 "Ch3" Ch3
        |> Codec.variant0 "Ch4" Ch4
        |> Codec.variant0 "Ch5" Ch5
        |> Codec.variant0 "Ch6" Ch6
        |> Codec.variant0 "Ch7" Ch7
        |> Codec.variant0 "Ch8" Ch8
        |> Codec.variant0 "Ch9" Ch9
        |> Codec.variant0 "Ch10" Ch10
        |> Codec.variant0 "Ch11" Ch11
        |> Codec.variant0 "Ch12" Ch12
        |> Codec.variant0 "Ch13" Ch13
        |> Codec.variant0 "Ch14" Ch14
        |> Codec.variant0 "Ch15" Ch15
        |> Codec.variant0 "Ch16" Ch16
        |> Codec.buildCustom


stringToChannel : String -> Maybe Channel
stringToChannel string =
    case string of
        "1" ->
            Just Ch1

        "2" ->
            Just Ch2

        "3" ->
            Just Ch3

        "4" ->
            Just Ch4

        "5" ->
            Just Ch5

        "6" ->
            Just Ch6

        "7" ->
            Just Ch7

        "8" ->
            Just Ch8

        "9" ->
            Just Ch9

        "10" ->
            Just Ch10

        "11" ->
            Just Ch11

        "12" ->
            Just Ch12

        "13" ->
            Just Ch13

        "14" ->
            Just Ch14

        "15" ->
            Just Ch15

        "16" ->
            Just Ch16

        _ ->
            Nothing


channelToString : Channel -> String
channelToString ch =
    case ch of
        Ch1 ->
            "1"

        Ch2 ->
            "2"

        Ch3 ->
            "3"

        Ch4 ->
            "4"

        Ch5 ->
            "5"

        Ch6 ->
            "6"

        Ch7 ->
            "7"

        Ch8 ->
            "8"

        Ch9 ->
            "9"

        Ch10 ->
            "10"

        Ch11 ->
            "11"

        Ch12 ->
            "12"

        Ch13 ->
            "13"

        Ch14 ->
            "14"

        Ch15 ->
            "15"

        Ch16 ->
            "16"


channelToMidiNumber : Channel -> Int
channelToMidiNumber ch =
    case ch of
        Ch1 ->
            0

        Ch2 ->
            1

        Ch3 ->
            2

        Ch4 ->
            3

        Ch5 ->
            4

        Ch6 ->
            5

        Ch7 ->
            6

        Ch8 ->
            7

        Ch9 ->
            8

        Ch10 ->
            9

        Ch11 ->
            10

        Ch12 ->
            11

        Ch13 ->
            12

        Ch14 ->
            13

        Ch15 ->
            14

        Ch16 ->
            15


intToChannel : Int -> Maybe Channel
intToChannel n =
    case n of
        0 ->
            Ch1 |> Just

        1 ->
            Ch2 |> Just

        2 ->
            Ch3 |> Just

        3 ->
            Ch4 |> Just

        4 ->
            Ch5 |> Just

        5 ->
            Ch6 |> Just

        6 ->
            Ch7 |> Just

        7 ->
            Ch8 |> Just

        8 ->
            Ch9 |> Just

        9 ->
            Ch10 |> Just

        10 ->
            Ch11 |> Just

        11 ->
            Ch12 |> Just

        12 ->
            Ch13 |> Just

        13 ->
            Ch14 |> Just

        14 ->
            Ch15 |> Just

        15 ->
            Ch16 |> Just

        _ ->
            Nothing



-- }}}
