module Controller exposing (..)

import Codec exposing (Codec)
import Midi exposing (MidiMsg(..))
import Style exposing (AppColour(..))


type Controller
    = Module String Controller
    | Row (List Controller)
    | Column (List Controller)
    | Note NoteState
    | CCValue CCValueState
    | Fader FaderState
    | MidiLog
    | Space


controllerCodec : Codec Controller
controllerCodec =
    Codec.recursive
        (\rmeta ->
            Codec.custom
                (\mod row col note ccv fad mid spa value ->
                    case value of
                        Module l c ->
                            mod l c

                        Row cs ->
                            row cs

                        Column cs ->
                            col cs

                        Note s ->
                            note s

                        CCValue s ->
                            ccv s

                        Fader s ->
                            fad s

                        MidiLog ->
                            mid

                        Space ->
                            spa
                )
                |> Codec.variant2 "Module" Module Codec.string rmeta
                |> Codec.variant1 "Row" Row (Codec.list rmeta)
                |> Codec.variant1 "Column" Column (Codec.list rmeta)
                |> Codec.variant1 "Note" Note noteStateCodec
                |> Codec.variant1 "CCValue" CCValue ccValueStateCodec
                |> Codec.variant1 "Fader" Fader faderStateCodec
                |> Codec.variant0 "MidiLog" MidiLog
                |> Codec.variant0 "Space" Space
                |> Codec.buildCustom
        )


controllerToString : Controller -> String
controllerToString control =
    case control of
        Module label _ ->
            "Module: " ++ label

        Row subcontrols ->
            "Row: " ++ (String.fromInt <| List.length subcontrols) ++ " items"

        Column subcontrols ->
            "Column: " ++ (String.fromInt <| List.length subcontrols) ++ " items"

        Note { channel, pitch, velocity } ->
            "Button: "
                ++ channelToString channel
                ++ " "
                ++ String.fromInt pitch
                ++ " "
                ++ String.fromInt velocity

        CCValue { channel, controller, value } ->
            "CC Value: "
                ++ channelToString channel
                ++ " "
                ++ String.fromInt controller
                ++ " "
                ++ String.fromInt value

        Fader { channel, ccNumber } ->
            "Fader: "
                ++ channelToString channel
                ++ " "
                ++ String.fromInt ccNumber

        MidiLog ->
            "MidiLog"

        Space ->
            "Space"


type alias NoteState =
    { status : ButtonStatus
    , label : String
    , colour : AppColour
    , channel : Channel
    , pitch : Int
    , velocity : Int
    }


noteStateCodec : Codec NoteState
noteStateCodec =
    Codec.object NoteState
        |> Codec.field "status" .status (Codec.constant Off)
        |> Codec.field "label" .label Codec.string
        |> Codec.field "colour" .colour Style.appColourCodec
        |> Codec.field "channel" .channel channelCodec
        |> Codec.field "pitch" .pitch Codec.int
        |> Codec.field "velocity" .velocity Codec.int
        |> Codec.buildObject


type ButtonStatus
    = On
    | Off


type alias CCValueState =
    { status : ButtonStatus
    , label : String
    , colour : AppColour
    , channel : Channel
    , controller : Int
    , value : Int
    }


ccValueStateCodec : Codec CCValueState
ccValueStateCodec =
    Codec.object CCValueState
        |> Codec.field "status" .status (Codec.constant Off)
        |> Codec.field "label" .label Codec.string
        |> Codec.field "colour" .colour Style.appColourCodec
        |> Codec.field "channel" .channel channelCodec
        |> Codec.field "controller" .controller Codec.int
        |> Codec.field "value" .value Codec.int
        |> Codec.buildObject


type alias FaderState =
    { status : FaderStatus
    , label : String
    , colour : AppColour
    , channel : Channel
    , ccNumber : Int
    , valuePercent : Int
    , valueMin : Int
    , valueMax : Int
    }


faderStateCodec : Codec FaderState
faderStateCodec =
    Codec.object FaderState
        |> Codec.field "status" .status (Codec.constant Set)
        |> Codec.field "label" .label Codec.string
        |> Codec.field "colour" .colour Style.appColourCodec
        |> Codec.field "channel" .channel channelCodec
        |> Codec.field "ccNumber" .ccNumber Codec.int
        |> Codec.field "valuePercent" .valuePercent Codec.int
        |> Codec.field "valueMin" .valueMin Codec.int
        |> Codec.field "valueMax" .valueMax Codec.int
        |> Codec.buildObject


type FaderStatus
    = Set
    | Changing Int ( Float, Float )


faderChanging : Int -> ( Float, Float ) -> Controller -> ( Controller, Maybe Midi.MidiMsg )
faderChanging identifier ( newX, newY ) controller =
    case controller of
        Fader state ->
            case state.status of
                Changing oldIdentifier ( _, oldY ) ->
                    if identifier == oldIdentifier then
                        let
                            valueChange =
                                oldY - newY |> round

                            newPercent =
                                state.valuePercent
                                    + valueChange
                                    |> clamp 0 100

                            value =
                                (127 // 100) * newPercent
                        in
                        ( Fader
                            { state
                                | status = Changing oldIdentifier ( newX, newY )
                                , valuePercent = newPercent
                            }
                        , Midi.ControllerChange
                            { channel = channelToMidiNumber state.channel
                            , controller = state.ccNumber
                            , value = value
                            }
                            |> Just
                        )

                    else
                        ( Fader state, Nothing )

                Set ->
                    ( Fader { state | status = Changing identifier ( newX, newY ) }, Nothing )

        _ ->
            ( controller, Nothing )


faderSet : Controller -> Controller
faderSet controller =
    case controller of
        Fader state ->
            Fader { state | status = Set }

        _ ->
            controller


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


midiNumberToChannel : Int -> Maybe Channel
midiNumberToChannel n =
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


type EditOperation
    = EditContainer
    | Add
    | Remove


getWithId : String -> String -> Controller -> Maybe Controller
getWithId currentId id control =
    case control of
        Module _ subController ->
            if currentId == id then
                Just control

            else
                getWithId (currentId ++ "_0") id subController

        Row controllers ->
            if currentId == id then
                Just control

            else
                List.indexedMap
                    (\i c ->
                        getWithId (currentId ++ "_" ++ String.fromInt i) id c
                    )
                    controllers
                    |> List.filterMap identity
                    |> List.head

        Column controllers ->
            if currentId == id then
                Just control

            else
                List.indexedMap
                    (\i c ->
                        getWithId (currentId ++ "_" ++ String.fromInt i) id c
                    )
                    controllers
                    |> List.filterMap identity
                    |> List.head

        Note _ ->
            if currentId == id then
                Just control

            else
                Nothing

        CCValue _ ->
            if currentId == id then
                Just control

            else
                Nothing

        Fader _ ->
            if currentId == id then
                Just control

            else
                Nothing

        MidiLog ->
            if currentId == id then
                Just control

            else
                Nothing

        Space ->
            if currentId == id then
                Just control

            else
                Nothing


updateWithId : String -> Controller -> { id : String, updateFn : Controller -> Controller } -> Controller
updateWithId currentId toUpdate updateInfo =
    let
        { id, updateFn } =
            updateInfo
    in
    case toUpdate of
        Module label controller ->
            if currentId == id then
                updateFn controller

            else
                updateWithId (currentId ++ "_0") controller updateInfo
                    |> Module label

        Row controllers ->
            if currentId == id then
                updateFn toUpdate

            else
                List.indexedMap
                    (\i c ->
                        updateWithId (currentId ++ "_" ++ String.fromInt i) c updateInfo
                    )
                    controllers
                    |> Row

        Column controllers ->
            if currentId == id then
                updateFn toUpdate

            else
                List.indexedMap
                    (\i c ->
                        updateWithId (currentId ++ "_" ++ String.fromInt i) c updateInfo
                    )
                    controllers
                    |> Column

        Note state ->
            if currentId == id then
                updateFn toUpdate

            else
                Note state

        CCValue state ->
            if currentId == id then
                updateFn toUpdate

            else
                CCValue state

        Fader state ->
            if currentId == id then
                updateFn toUpdate

            else
                Fader state

        MidiLog ->
            if currentId == id then
                updateFn toUpdate

            else
                MidiLog

        Space ->
            if currentId == id then
                updateFn toUpdate

            else
                Space


addSpace : Controller -> Controller
addSpace controller =
    case controller of
        Row subControls ->
            subControls
                ++ [ Space ]
                |> Row

        Column subControls ->
            subControls
                ++ [ Space ]
                |> Column

        _ ->
            controller


removeItem : Controller -> Controller
removeItem controller =
    case controller of
        Row subControls ->
            subControls
                |> List.reverse
                |> List.drop 1
                |> List.reverse
                |> Row

        Column subControls ->
            subControls
                |> List.reverse
                |> List.drop 1
                |> List.reverse
                |> Column

        _ ->
            controller


newNote : String -> AppColour -> Channel -> Int -> Int -> Controller
newNote label colour channel noteNumber velocity =
    Note
        { status = Off
        , label = label
        , colour = colour
        , channel = channel
        , pitch = noteNumber
        , velocity = velocity
        }


newCCValue : String -> AppColour -> Channel -> Int -> Int -> Controller
newCCValue label colour channel controller value =
    CCValue
        { status = Off
        , colour = colour
        , label = label
        , channel = channel
        , controller = controller
        , value = value
        }


buttonOn : Controller -> ( Controller, Maybe Midi.MidiMsg )
buttonOn controller =
    case controller of
        Note state ->
            ( Note { state | status = On }
            , Midi.NoteOn
                { channel = channelToMidiNumber state.channel
                , pitch = state.pitch
                , velocity = state.velocity
                }
                |> Just
            )

        CCValue state ->
            ( CCValue { state | status = On }
            , Midi.ControllerChange
                { channel = channelToMidiNumber state.channel
                , controller = state.controller
                , value = state.value
                }
                |> Just
            )

        _ ->
            ( controller, Nothing )


buttonOff : Controller -> ( Controller, Maybe Midi.MidiMsg )
buttonOff controller =
    case controller of
        Note state ->
            ( Note { state | status = Off }
            , Midi.NoteOff
                { channel = channelToMidiNumber state.channel
                , pitch = state.pitch
                , velocity = 0
                }
                |> Just
            )

        CCValue state ->
            ( CCValue { state | status = Off }
            , Nothing
            )

        _ ->
            ( controller, Nothing )
