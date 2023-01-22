module Midi exposing (..)

import Array exposing (Array)
import Bitwise
import Codec exposing (Codec)
import Element exposing (..)
import Element.Border as Border
import Element.Input as Input
import Style exposing (..)



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


midiMsgCodec : Codec MidiMsg
midiMsgCodec =
    let
        noteParamsCodec : Codec { channel : Int, pitch : Int, velocity : Int }
        noteParamsCodec =
            Codec.object (\ch p v -> { channel = ch, pitch = p, velocity = v })
                |> Codec.field "channel" .channel Codec.int
                |> Codec.field "pitch" .pitch Codec.int
                |> Codec.field "velocity" .velocity Codec.int
                |> Codec.buildObject

        keyPressureParamsCodec : Codec { channel : Int, key : Int, pressure : Int }
        keyPressureParamsCodec =
            Codec.object (\ch k p -> { channel = ch, key = k, pressure = p })
                |> Codec.field "channel" .channel Codec.int
                |> Codec.field "key" .key Codec.int
                |> Codec.field "pressure" .pressure Codec.int
                |> Codec.buildObject

        ccParamsCodec : Codec { channel : Int, controller : Int, value : Int }
        ccParamsCodec =
            Codec.object (\ch c v -> { channel = ch, controller = c, value = v })
                |> Codec.field "channel" .channel Codec.int
                |> Codec.field "controller" .controller Codec.int
                |> Codec.field "value" .value Codec.int
                |> Codec.buildObject

        pcParamsCodec : Codec { channel : Int, preset : Int }
        pcParamsCodec =
            Codec.object (\ch p -> { channel = ch, preset = p })
                |> Codec.field "channel" .channel Codec.int
                |> Codec.field "preset" .preset Codec.int
                |> Codec.buildObject

        chPresParamsCodec : Codec { channel : Int, pressure : Int }
        chPresParamsCodec =
            Codec.object (\ch p -> { channel = ch, pressure = p })
                |> Codec.field "channel" .channel Codec.int
                |> Codec.field "pressure" .pressure Codec.int
                |> Codec.buildObject

        pitchBendParamsCodec : Codec { channel : Int, bendLSB : Int, bendMSB : Int }
        pitchBendParamsCodec =
            Codec.object (\ch l m -> { channel = ch, bendLSB = l, bendMSB = m })
                |> Codec.field "channel" .channel Codec.int
                |> Codec.field "bendLSB" .bendLSB Codec.int
                |> Codec.field "bendMSB" .bendMSB Codec.int
                |> Codec.buildObject

        sysExParamsCodec : Codec { vendorId : Int, data : Array Int }
        sysExParamsCodec =
            Codec.object (\v d -> { vendorId = v, data = d })
                |> Codec.field "vendorId" .vendorId Codec.int
                |> Codec.field "data" .data (Codec.array Codec.int)
                |> Codec.buildObject

        songPosParamsCodec : Codec { positionLSB : Int, positionMSB : Int }
        songPosParamsCodec =
            Codec.object (\l m -> { positionLSB = l, positionMSB = m })
                |> Codec.field "positionLSB" .positionLSB Codec.int
                |> Codec.field "positionMSB" .positionMSB Codec.int
                |> Codec.buildObject
    in
    Codec.custom
        (\non nof kp cc pc cp pb se sp ss ub tr es tt sta con sto act sre unk value ->
            case value of
                NoteOn params ->
                    non params

                NoteOff params ->
                    nof params

                KeyPressure params ->
                    kp params

                ControllerChange params ->
                    cc params

                ProgramChange params ->
                    pc params

                ChannelPressure params ->
                    cp params

                PitchBend params ->
                    pb params

                SystemExclusive params ->
                    se params

                SongPosition params ->
                    sp params

                SongSelect songNumber ->
                    ss songNumber

                UnofficialBusSelect busNumber ->
                    ub busNumber

                TuneRequest ->
                    tr

                EndOfSysEx ->
                    es

                TimingTick ->
                    tt

                StartSong ->
                    sta

                ContinueSong ->
                    con

                StopSong ->
                    sto

                ActiveSensing ->
                    act

                SystemReset ->
                    sre

                Unknown data ->
                    unk data
        )
        |> Codec.variant1 "NoteOn" NoteOn noteParamsCodec
        |> Codec.variant1 "NoteOff" NoteOff noteParamsCodec
        |> Codec.variant1 "KeyPressure" KeyPressure keyPressureParamsCodec
        |> Codec.variant1 "ControllerChange" ControllerChange ccParamsCodec
        |> Codec.variant1 "ProgramChange" ProgramChange pcParamsCodec
        |> Codec.variant1 "ChannelPressure" ChannelPressure chPresParamsCodec
        |> Codec.variant1 "PitchBend" PitchBend pitchBendParamsCodec
        |> Codec.variant1 "SystemExclusive" SystemExclusive sysExParamsCodec
        |> Codec.variant1 "SongPosition" SongPosition songPosParamsCodec
        |> Codec.variant1 "SongSelect" SongSelect Codec.int
        |> Codec.variant1 "UnofficialBusSelect" UnofficialBusSelect Codec.int
        |> Codec.variant0 "TuneRequest" TuneRequest
        |> Codec.variant0 "EndOfSysEx" EndOfSysEx
        |> Codec.variant0 "TimingTick" TimingTick
        |> Codec.variant0 "StartSong" StartSong
        |> Codec.variant0 "ContinueSong" ContinueSong
        |> Codec.variant0 "StopSong" StopSong
        |> Codec.variant0 "ActiveSensing" ActiveSensing
        |> Codec.variant0 "SystemReset" SystemReset
        |> Codec.variant1 "Unknown" Unknown (Codec.array Codec.int)
        |> Codec.buildCustom


midiMsgToString : MidiMsg -> String
midiMsgToString midiMsg =
    case midiMsg of
        NoteOn { channel, pitch, velocity } ->
            "Note On: Ch "
                ++ String.fromInt channel
                ++ ", Pit "
                ++ String.fromInt pitch
                ++ ", Vel "
                ++ String.fromInt velocity

        NoteOff { channel, pitch, velocity } ->
            "Note Off: Ch "
                ++ String.fromInt channel
                ++ ", Pit "
                ++ String.fromInt pitch
                ++ ", Vel "
                ++ String.fromInt velocity

        KeyPressure { channel, key, pressure } ->
            "Key Pressure: Ch "
                ++ String.fromInt channel
                ++ ", Key "
                ++ String.fromInt key
                ++ ", Pres "
                ++ String.fromInt pressure

        ControllerChange { channel, controller, value } ->
            "CC: Ch "
                ++ String.fromInt channel
                ++ ", Con "
                ++ String.fromInt controller
                ++ ", Val "
                ++ String.fromInt value

        ProgramChange { channel, preset } ->
            "Program Change: Ch "
                ++ String.fromInt channel
                ++ ", Pre "
                ++ String.fromInt preset

        ChannelPressure { channel, pressure } ->
            "Channel Pressure: Ch "
                ++ String.fromInt channel
                ++ ", Pres "
                ++ String.fromInt pressure

        PitchBend { channel, bendLSB, bendMSB } ->
            "Pitch Bend: Ch "
                ++ String.fromInt channel
                ++ ", comb "
                ++ String.fromInt (bendLSB + Bitwise.shiftLeftBy 7 bendMSB)

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
            "Song Select:  Song# "
                ++ String.fromInt songNumber

        UnofficialBusSelect busNumber ->
            "Unofficial Bus Select: Bus# "
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


changeChannel : Int -> MidiMsg -> MidiMsg
changeChannel newChannel midiMsg =
    case midiMsg of
        NoteOn params ->
            NoteOn { params | channel = newChannel }

        NoteOff params ->
            NoteOff { params | channel = newChannel }

        KeyPressure params ->
            KeyPressure { params | channel = newChannel }

        ControllerChange params ->
            ControllerChange { params | channel = newChannel }

        ProgramChange params ->
            ProgramChange { params | channel = newChannel }

        ChannelPressure params ->
            ChannelPressure { params | channel = newChannel }

        PitchBend params ->
            PitchBend { params | channel = newChannel }

        SystemExclusive params ->
            SystemExclusive params

        SongPosition params ->
            SongPosition params

        SongSelect params ->
            SongSelect params

        UnofficialBusSelect params ->
            UnofficialBusSelect params

        TuneRequest ->
            TuneRequest

        EndOfSysEx ->
            EndOfSysEx

        TimingTick ->
            TimingTick

        StartSong ->
            StartSong

        ContinueSong ->
            ContinueSong

        StopSong ->
            StopSong

        ActiveSensing ->
            ActiveSensing

        SystemReset ->
            SystemReset

        Unknown data ->
            Unknown data


intToMsbLsb : Int -> { lsb : Int, msb : Int }
intToMsbLsb input =
    let
        clamped =
            clamp 0 16383 input
    in
    { lsb = 0 -- Bitwise.and clamped 0xFF
    , msb =
        Bitwise.and clamped 0xFF00
            |> Bitwise.shiftRightBy 7
    }



-- }}}
-- {{{ EditMidiMsg


type EditMidiButtonMsg
    = ENoteOn { channel : String, pitch : String, velocity : String }
    | ENoteOff { channel : String, pitch : String, velocity : String }
    | EControllerChange { channel : String, controller : String, value : String }
    | EProgramChange { channel : String, preset : String }
    | ESongSelect String
      -- Some message types are commented out as they seem to break web midi
      -- | EUnofficialBusSelect String
    | ETuneRequest
      -- | EEndOfSysEx
      -- | ETimingTick
    | EStartSong
    | EContinueSong
    | EStopSong
    | EActiveSensing
    | ESystemReset


editMidiButtonToMidiMsg : EditMidiButtonMsg -> Maybe MidiMsg
editMidiButtonToMidiMsg eMidiButtonMsg =
    let
        toByteInt i =
            String.toInt i
                |> Maybe.map (clamp 0 127)

        toNibbleInt i =
            String.toInt i
                |> Maybe.map (\ch -> ch - 1)
                |> Maybe.map (clamp 0 15)
    in
    case eMidiButtonMsg of
        ENoteOn { channel, pitch, velocity } ->
            case ( toNibbleInt channel, toByteInt pitch, toByteInt velocity ) of
                ( Just ch, Just p, Just v ) ->
                    Just <| NoteOn { channel = ch, pitch = p, velocity = v }

                _ ->
                    Nothing

        ENoteOff { channel, pitch, velocity } ->
            case ( toNibbleInt channel, toByteInt pitch, toByteInt velocity ) of
                ( Just ch, Just p, Just v ) ->
                    Just <| NoteOff { channel = ch, pitch = p, velocity = v }

                _ ->
                    Nothing

        EControllerChange { channel, controller, value } ->
            case ( toNibbleInt channel, toByteInt controller, toByteInt value ) of
                ( Just ch, Just c, Just v ) ->
                    Just <|
                        ControllerChange
                            { channel = ch
                            , controller = c
                            , value = v
                            }

                _ ->
                    Nothing

        EProgramChange { channel, preset } ->
            case ( toNibbleInt channel, toByteInt preset ) of
                ( Just ch, Just p ) ->
                    Just <|
                        ProgramChange
                            { channel = ch
                            , preset = p
                            }

                _ ->
                    Nothing

        ESongSelect songNumber ->
            toByteInt songNumber
                |> Maybe.map SongSelect

        -- EUnofficialBusSelect busNumber ->
        --     toByteInt busNumber
        --         |> Maybe.map UnofficialBusSelect
        ETuneRequest ->
            Just TuneRequest

        -- EEndOfSysEx ->
        --     Just EndOfSysEx
        -- ETimingTick ->
        --     Just TimingTick
        EStartSong ->
            Just StartSong

        EContinueSong ->
            Just ContinueSong

        EStopSong ->
            Just StopSong

        EActiveSensing ->
            Just ActiveSensing

        ESystemReset ->
            Just SystemReset


editMidiButtonSelector : (EditMidiButtonMsg -> msg) -> EditMidiButtonMsg -> Element msg
editMidiButtonSelector selectMsg selected =
    Input.radio
        [ spacing 4
        , padding 4
        , width fill
        , height (px 100)
        , scrollbarY
        , Border.width 2
        ]
        { onChange = selectMsg
        , selected = Just selected
        , label = Input.labelHidden "Message Type"
        , options =
            [ Input.option
                (ENoteOn { channel = "", pitch = "", velocity = "" })
                (text "Note On")
            , Input.option
                (ENoteOff { channel = "", pitch = "", velocity = "" })
                (text "Note Off")
            , Input.option
                (EControllerChange { channel = "", controller = "", value = "" })
                (text "CC Value")
            , Input.option
                (EProgramChange { channel = "", preset = "" })
                (text "Program Change")
            , Input.option
                (ESongSelect "")
                (text "Song Select")

            -- , Input.option
            --     (EUnofficialBusSelect "")
            --     (text "Unofficial Bus Select")
            , Input.option
                ETuneRequest
                (text "Tune Request")

            -- , Input.option
            --     EEndOfSysEx
            --     (text "End of Sys Ex")
            -- , Input.option
            --     ETimingTick
            --     (text "Timing Tick")
            , Input.option
                EStartSong
                (text "Start Song")
            , Input.option
                EContinueSong
                (text "Continue Song")
            , Input.option
                EStopSong
                (text "Stop Song")
            , Input.option
                EActiveSensing
                (text "Active Sensing")
            , Input.option
                ESystemReset
                (text "System Reset")
            ]
        }


editMidiButtonMsgView : (EditMidiButtonMsg -> msg) -> EditMidiButtonMsg -> Element msg
editMidiButtonMsgView editMsg midiButtonMsg =
    column
        [ padding 4, spacing 4, width fill ]
        [ case midiButtonMsg of
            ENoteOn state ->
                column
                    [ spacing 4, width fill ]
                    [ editTextBox
                        { placeholder = "1-16"
                        , label = "Channel"
                        , current = state.channel
                        }
                        "number"
                        (\newChannel ->
                            { state | channel = newChannel }
                                |> ENoteOn
                                |> editMsg
                        )
                    , editTextBox
                        { placeholder = "0-127"
                        , label = "Pitch"
                        , current = state.pitch
                        }
                        "number"
                        (\newPitch ->
                            { state | pitch = newPitch }
                                |> ENoteOn
                                |> editMsg
                        )
                    , editTextBox
                        { placeholder = "0-127"
                        , label = "Velocity"
                        , current = state.pitch
                        }
                        "number"
                        (\newPitch ->
                            { state | pitch = newPitch }
                                |> ENoteOn
                                |> editMsg
                        )
                    ]

            ENoteOff state ->
                column
                    [ spacing 4, width fill ]
                    [ editTextBox
                        { placeholder = "1-16"
                        , label = "Channel"
                        , current = state.channel
                        }
                        "number"
                        (\newChannel ->
                            { state | channel = newChannel }
                                |> ENoteOff
                                |> editMsg
                        )
                    , editTextBox
                        { placeholder = "0-127"
                        , label = "Pitch"
                        , current = state.pitch
                        }
                        "number"
                        (\newPitch ->
                            { state | pitch = newPitch }
                                |> ENoteOff
                                |> editMsg
                        )
                    , editTextBox
                        { placeholder = "0-127"
                        , label = "Velocity"
                        , current = state.pitch
                        }
                        "number"
                        (\newPitch ->
                            { state | pitch = newPitch }
                                |> ENoteOff
                                |> editMsg
                        )
                    ]

            EControllerChange state ->
                column
                    [ spacing 4, width fill ]
                    [ editTextBox
                        { placeholder = "1-16"
                        , label = "Channel"
                        , current = state.channel
                        }
                        "number"
                        (\newChannel ->
                            { state | channel = newChannel }
                                |> EControllerChange
                                |> editMsg
                        )
                    , editTextBox
                        { placeholder = "0-127"
                        , label = "CC#"
                        , current = state.controller
                        }
                        "number"
                        (\newCC ->
                            { state | controller = newCC }
                                |> EControllerChange
                                |> editMsg
                        )
                    , editTextBox
                        { placeholder = "0-127"
                        , label = "Value"
                        , current = state.value
                        }
                        "number"
                        (\newValue ->
                            { state | value = newValue }
                                |> EControllerChange
                                |> editMsg
                        )
                    ]

            EProgramChange state ->
                column
                    [ spacing 4, width fill ]
                    [ editTextBox
                        { placeholder = "1-16"
                        , label = "Channel"
                        , current = state.channel
                        }
                        "number"
                        (\newChannel ->
                            { state | channel = newChannel }
                                |> EProgramChange
                                |> editMsg
                        )
                    , editTextBox
                        { placeholder = "0-127"
                        , label = "Preset"
                        , current = state.preset
                        }
                        "number"
                        (\newPreset ->
                            { state | preset = newPreset }
                                |> EProgramChange
                                |> editMsg
                        )
                    ]

            ESongSelect songNumber ->
                column
                    [ spacing 4, width fill ]
                    [ editTextBox
                        { placeholder = "0-127"
                        , label = "Song Number"
                        , current = songNumber
                        }
                        "number"
                        (\newSongNumber ->
                            ESongSelect newSongNumber
                                |> editMsg
                        )
                    ]

            -- EUnofficialBusSelect busNumber ->
            --     column
            --         [ spacing 4 ]
            --         [ editTextBox
            --             { placeholder = "0-127"
            --             , label = "Bus Number"
            --             , current = busNumber
            --             }
            --             "number"
            --             (\newBusNumber ->
            --                 EUnofficialBusSelect newBusNumber
            --                     |> editMsg
            --             )
            --         ]
            ETuneRequest ->
                none

            -- EEndOfSysEx ->
            --     none
            -- ETimingTick ->
            --     none
            EStartSong ->
                none

            EContinueSong ->
                none

            EStopSong ->
                none

            EActiveSensing ->
                none

            ESystemReset ->
                none
        ]


midiMsgToEditMidiButtonMsg : MidiMsg -> Maybe EditMidiButtonMsg
midiMsgToEditMidiButtonMsg midiMsg =
    case midiMsg of
        NoteOn { channel, pitch, velocity } ->
            ENoteOn
                -- Adding 1 to channel for readability
                { channel = String.fromInt (channel + 1)
                , pitch = String.fromInt pitch
                , velocity = String.fromInt velocity
                }
                |> Just

        NoteOff { channel, pitch, velocity } ->
            ENoteOff
                -- Adding 1 to channel for readability
                { channel = String.fromInt (channel + 1)
                , pitch = String.fromInt pitch
                , velocity = String.fromInt velocity
                }
                |> Just

        ControllerChange { channel, controller, value } ->
            EControllerChange
                { channel = String.fromInt (channel + 1)
                , controller = String.fromInt controller
                , value = String.fromInt value
                }
                |> Just

        ProgramChange { channel, preset } ->
            EProgramChange
                -- Adding 1 to channel for readability
                { channel = String.fromInt (channel + 1)
                , preset = String.fromInt preset
                }
                |> Just

        SongSelect songNumber ->
            ESongSelect (String.fromInt songNumber)
                |> Just

        -- UnofficialBusSelect busNumber ->
        --     EUnofficialBusSelect (String.fromInt busNumber)
        --         |> Just
        TuneRequest ->
            ETuneRequest
                |> Just

        -- EndOfSysEx ->
        --     EEndOfSysEx
        --         |> Just
        -- TimingTick ->
        --     ETimingTick
        --         |> Just
        StartSong ->
            EStartSong
                |> Just

        ContinueSong ->
            EContinueSong
                |> Just

        StopSong ->
            EStopSong
                |> Just

        ActiveSensing ->
            EActiveSensing
                |> Just

        SystemReset ->
            ESystemReset
                |> Just

        _ ->
            Nothing



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
