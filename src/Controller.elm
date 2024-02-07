module Controller exposing (..)

import Array exposing (Array)
import Codec exposing (Codec)
import Element exposing (..)
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import FeatherIcons as Icons
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Midi exposing (Channel(..), MidiMsg(..))
import Style exposing (..)
import Utils exposing (PageConfig)


type Controller
    = Module String Controller
    | Row (List Controller)
    | Column (List Controller)
    | SizedRow (List ( Int, Controller ))
    | SizedColumn (List ( Int, Controller ))
    | Note NoteState
    | Chord ChordState
    | CCValue CCValueState
    | Command CommandState
    | Sequence SequenceState
    | Fader FaderState
    | XYFader XYFaderState
    | PitchBend PitchBendState
    | MidiLog
    | Space


controllerCodec : Codec Controller
controllerCodec =
    Codec.recursive
        (\rmeta ->
            Codec.custom
                (\mod row col srw scl note cho ccv com seq fad xy pb mid spa value ->
                    case value of
                        Module l c ->
                            mod l c

                        Row cs ->
                            row cs

                        Column cs ->
                            col cs

                        SizedRow cs ->
                            srw cs

                        SizedColumn cs ->
                            scl cs

                        Note s ->
                            note s

                        Chord s ->
                            cho s

                        CCValue s ->
                            ccv s

                        Command s ->
                            com s

                        Sequence s ->
                            seq s

                        Fader s ->
                            fad s

                        XYFader s ->
                            xy s

                        PitchBend s ->
                            pb s

                        MidiLog ->
                            mid

                        Space ->
                            spa
                )
                |> Codec.variant2 "Module" Module Codec.string rmeta
                |> Codec.variant1 "Row" Row (Codec.list rmeta)
                |> Codec.variant1 "Column" Column (Codec.list rmeta)
                |> Codec.variant1 "SizedRow" SizedRow (Codec.list (Codec.tuple Codec.int rmeta))
                |> Codec.variant1 "SizedColumn" SizedColumn (Codec.list (Codec.tuple Codec.int rmeta))
                |> Codec.variant1 "Note" Note noteStateCodec
                |> Codec.variant1 "Chord" Chord chordStateCodec
                |> Codec.variant1 "CCValue" CCValue ccValueStateCodec
                |> Codec.variant1 "Command" Command commandStateCodec
                |> Codec.variant1 "Sequence" Sequence sequenceStateCodec
                |> Codec.variant1 "Fader" Fader faderStateCodec
                |> Codec.variant1 "XYFader" XYFader xyFaderStateCodec
                |> Codec.variant1 "PitchBend" PitchBend pitchBendStateCodec
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

        SizedRow subcontrols ->
            "Row: " ++ (String.fromInt <| List.length subcontrols) ++ " items"

        SizedColumn subcontrols ->
            "Column: " ++ (String.fromInt <| List.length subcontrols) ++ " items"

        Note { channel, pitch, velocity } ->
            "Button: "
                ++ Midi.channelToString channel
                ++ " "
                ++ String.fromInt pitch
                ++ " "
                ++ String.fromInt velocity

        Chord { notes } ->
            "Chord: " ++ (List.length notes |> String.fromInt) ++ " notes"

        CCValue { channel, controller, value } ->
            "CC Value: "
                ++ Midi.channelToString channel
                ++ " "
                ++ String.fromInt controller
                ++ " "
                ++ String.fromInt value

        Command { onPressMsgs, onReleaseMsgs } ->
            "Command: "
                ++ String.fromInt (List.length onPressMsgs)
                ++ " msgs on press, "
                ++ String.fromInt (List.length onReleaseMsgs)
                ++ " msgs on release "

        Sequence { midiMsgs } ->
            "Sequence: "
                ++ String.fromInt (Array.length midiMsgs)
                ++ " msgs"

        Fader { channel, ccNumber } ->
            "Fader: "
                ++ Midi.channelToString channel
                ++ " "
                ++ String.fromInt ccNumber

        XYFader { channel1, channel2, ccNumber1, ccNumber2 } ->
            "Fader: "
                ++ Midi.channelToString channel1
                ++ " cc1 "
                ++ String.fromInt ccNumber1
                ++ " "
                ++ Midi.channelToString channel2
                ++ " cc2 "
                ++ String.fromInt ccNumber2

        PitchBend { channel } ->
            "PitchBend: Ch "
                ++ Midi.channelToString channel

        MidiLog ->
            "MidiLog"

        Space ->
            "Space"


type alias NoteState =
    { status : ButtonStatus
    , label : String
    , labelSize : Maybe LabelSize
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
        |> Codec.maybeField "labelSize" .labelSize labelSizeCodec
        |> Codec.field "colour" .colour Style.appColourCodec
        |> Codec.field "channel" .channel Midi.channelCodec
        |> Codec.field "pitch" .pitch Codec.int
        |> Codec.field "velocity" .velocity Codec.int
        |> Codec.buildObject


type alias ChordState =
    { status : ButtonStatus
    , label : String
    , labelSize : Maybe LabelSize
    , colour : AppColour
    , velocity : Int
    , notes :
        List
            { channel : Channel
            , pitch : Int
            }
    }


chordStateCodec : Codec ChordState
chordStateCodec =
    let
        noteCodec =
            Codec.object (\c p -> { channel = c, pitch = p })
                |> Codec.field "channel" .channel Midi.channelCodec
                |> Codec.field "pitch" .pitch Codec.int
                |> Codec.buildObject
    in
    Codec.object ChordState
        |> Codec.field "status" .status (Codec.constant Off)
        |> Codec.field "label" .label Codec.string
        |> Codec.maybeField "labelSize" .labelSize labelSizeCodec
        |> Codec.field "colour" .colour Style.appColourCodec
        |> Codec.field "velocity" .velocity Codec.int
        |> Codec.field "notes" .notes (Codec.list noteCodec)
        |> Codec.buildObject


type ButtonStatus
    = On
    | Off


type alias CCValueState =
    { status : ButtonStatus
    , label : String
    , labelSize : Maybe LabelSize
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
        |> Codec.maybeField "labelSize" .labelSize labelSizeCodec
        |> Codec.field "colour" .colour Style.appColourCodec
        |> Codec.field "channel" .channel Midi.channelCodec
        |> Codec.field "controller" .controller Codec.int
        |> Codec.field "value" .value Codec.int
        |> Codec.buildObject


type alias CommandState =
    { status : ButtonStatus
    , label : String
    , labelSize : Maybe LabelSize
    , colour : AppColour
    , onPressMsgs : List MidiMsg
    , onReleaseMsgs : List MidiMsg
    }


commandStateCodec : Codec CommandState
commandStateCodec =
    Codec.object CommandState
        |> Codec.field "status" .status (Codec.constant Off)
        |> Codec.field "label" .label Codec.string
        |> Codec.maybeField "labelSize" .labelSize labelSizeCodec
        |> Codec.field "colour" .colour Style.appColourCodec
        |> Codec.field "onPressMsgs" .onPressMsgs (Codec.list Midi.midiMsgCodec)
        |> Codec.field "onReleaseMsgs" .onReleaseMsgs (Codec.list Midi.midiMsgCodec)
        |> Codec.buildObject


type alias SequenceState =
    { status : ButtonStatus
    , label : String
    , labelSize : Maybe LabelSize
    , colour : AppColour
    , midiMsgs : Array MidiMsg
    , index : Int
    }


sequenceStateCodec : Codec SequenceState
sequenceStateCodec =
    Codec.object SequenceState
        |> Codec.field "status" .status (Codec.constant Off)
        |> Codec.field "label" .label Codec.string
        |> Codec.maybeField "labelSize" .labelSize labelSizeCodec
        |> Codec.field "colour" .colour Style.appColourCodec
        |> Codec.field "midiMsgs" .midiMsgs (Codec.array Midi.midiMsgCodec)
        |> Codec.field "index" .index Codec.int
        |> Codec.buildObject


type alias FaderState =
    { status : FaderStatus
    , label : String
    , labelSize : Maybe LabelSize
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
        |> Codec.maybeField "labelSize" .labelSize labelSizeCodec
        |> Codec.field "colour" .colour Style.appColourCodec
        |> Codec.field "channel" .channel Midi.channelCodec
        |> Codec.field "ccNumber" .ccNumber Codec.int
        |> Codec.field "valuePercent" .valuePercent Codec.int
        |> Codec.field "valueMin" .valueMin Codec.int
        |> Codec.field "valueMax" .valueMax Codec.int
        |> Codec.buildObject


type FaderStatus
    = Set
    | Changing Int ( Float, Float )


faderChanging : Int -> ( Float, Float ) -> Controller -> ( Controller, List Midi.MidiMsg )
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

                            protectedMax =
                                -- This is here to unsure that the first presets that I
                                -- made still work. I stupidly set their max value to 0.
                                if state.valueMax == 0 then
                                    127

                                else
                                    state.valueMax

                            rangeValue =
                                (protectedMax - state.valueMin)
                                    |> toFloat

                            value =
                                ((rangeValue / 100) * toFloat newPercent)
                                    |> round
                                    |> (+) state.valueMin
                                    |> clamp 0 127
                        in
                        ( Fader
                            { state
                                | status = Changing oldIdentifier ( newX, newY )
                                , valuePercent = newPercent
                            }
                        , [ Midi.ControllerChange
                                { channel = Midi.channelToMidiNumber state.channel
                                , controller = state.ccNumber
                                , value = value
                                }
                          ]
                        )

                    else
                        ( Fader state, [] )

                Set ->
                    ( Fader { state | status = Changing identifier ( newX, newY ) }, [] )

        XYFader state ->
            case state.status of
                Changing oldIdentifier ( oldX, oldY ) ->
                    if identifier == oldIdentifier then
                        let
                            valueChange1 =
                                newX - oldX |> round

                            newPercent1 =
                                state.valuePercent1
                                    + valueChange1
                                    |> clamp 0 100

                            protectedMax1 =
                                -- This is here to unsure that the first presets that I
                                -- made still work. I stupidly set their max value to 0.
                                if state.valueMax1 == 0 then
                                    127

                                else
                                    state.valueMax1

                            rangeValue1 =
                                (protectedMax1 - state.valueMin1)
                                    |> toFloat

                            value1 =
                                ((rangeValue1 / 100) * toFloat newPercent1)
                                    |> round
                                    |> (+) state.valueMin1
                                    |> clamp 0 127

                            valueChange2 =
                                oldY - newY |> round

                            newPercent2 =
                                state.valuePercent2
                                    + valueChange2
                                    |> clamp 0 100

                            protectedMax2 =
                                -- This is here to unsure that the first presets that I
                                -- made still work. I stupidly set their max value to 0.
                                if state.valueMax2 == 0 then
                                    127

                                else
                                    state.valueMax2

                            rangeValue2 =
                                (protectedMax2 - state.valueMin2)
                                    |> toFloat

                            value2 =
                                ((rangeValue2 / 100) * toFloat newPercent2)
                                    |> round
                                    |> (+) state.valueMin2
                                    |> clamp 0 127
                        in
                        ( XYFader
                            { state
                                | status = Changing oldIdentifier ( newX, newY )
                                , valuePercent1 = newPercent1
                                , valuePercent2 = newPercent2
                            }
                        , [ Midi.ControllerChange
                                { channel = Midi.channelToMidiNumber state.channel1
                                , controller = state.ccNumber1
                                , value = value1
                                }
                          , Midi.ControllerChange
                                { channel = Midi.channelToMidiNumber state.channel2
                                , controller = state.ccNumber2
                                , value = value2
                                }
                          ]
                        )

                    else
                        ( XYFader state, [] )

                Set ->
                    ( XYFader { state | status = Changing identifier ( newX, newY ) }, [] )

        PitchBend state ->
            case state.status of
                Changing oldIdentifier ( _, oldY ) ->
                    if identifier == oldIdentifier then
                        let
                            valueChange =
                                oldY - newY |> round

                            value =
                                state.bendValue
                                    + (valueChange * 80)

                            { lsb, msb } =
                                Midi.intToMsbLsb value
                        in
                        ( PitchBend
                            { state
                                | status = Changing oldIdentifier ( newX, newY )
                                , bendValue = value
                            }
                        , [ Midi.PitchBend
                                { channel = Midi.channelToMidiNumber state.channel
                                , bendLSB = lsb
                                , bendMSB = msb
                                }
                          ]
                        )

                    else
                        ( PitchBend state, [] )

                Set ->
                    ( PitchBend { state | status = Changing identifier ( newX, newY ) }, [] )

        _ ->
            ( controller, [] )


faderSet : Controller -> ( Controller, List MidiMsg )
faderSet controller =
    case controller of
        Fader state ->
            ( Fader { state | status = Set }
            , []
            )

        XYFader state ->
            ( XYFader { state | status = Set }
            , []
            )

        PitchBend state ->
            let
                defaultBend =
                    8192

                { lsb, msb } =
                    Midi.intToMsbLsb defaultBend
            in
            ( PitchBend
                { state
                    | status = Set
                    , bendValue = defaultBend
                }
            , [ Midi.PitchBend
                    { channel = Midi.channelToMidiNumber state.channel
                    , bendLSB = lsb
                    , bendMSB = msb
                    }
              ]
            )

        _ ->
            ( controller
            , []
            )


type alias XYFaderState =
    { status : FaderStatus
    , label : String
    , labelSize : Maybe LabelSize
    , colour : AppColour
    , channel1 : Channel
    , ccNumber1 : Int
    , valuePercent1 : Int
    , valueMin1 : Int
    , valueMax1 : Int
    , channel2 : Channel
    , ccNumber2 : Int
    , valuePercent2 : Int
    , valueMin2 : Int
    , valueMax2 : Int
    }


xyFaderStateCodec : Codec XYFaderState
xyFaderStateCodec =
    Codec.object XYFaderState
        |> Codec.field "status" .status (Codec.constant Set)
        |> Codec.field "label" .label Codec.string
        |> Codec.maybeField "labelSize" .labelSize labelSizeCodec
        |> Codec.field "colour" .colour Style.appColourCodec
        |> Codec.field "channel1" .channel1 Midi.channelCodec
        |> Codec.field "ccNumber1" .ccNumber1 Codec.int
        |> Codec.field "valuePercent1" .valuePercent1 Codec.int
        |> Codec.field "valueMin1" .valueMin1 Codec.int
        |> Codec.field "valueMax1" .valueMax1 Codec.int
        |> Codec.field "channel2" .channel2 Midi.channelCodec
        |> Codec.field "ccNumber2" .ccNumber2 Codec.int
        |> Codec.field "valuePercent2" .valuePercent2 Codec.int
        |> Codec.field "valueMin2" .valueMin2 Codec.int
        |> Codec.field "valueMax2" .valueMax2 Codec.int
        |> Codec.buildObject


type alias PitchBendState =
    { status : FaderStatus
    , label : String
    , labelSize : Maybe LabelSize
    , colour : AppColour
    , channel : Channel
    , bendValue : Int
    }


pitchBendStateCodec : Codec PitchBendState
pitchBendStateCodec =
    Codec.object PitchBendState
        |> Codec.field "status" .status (Codec.constant Set)
        |> Codec.field "label" .label Codec.string
        |> Codec.maybeField "labelSize" .labelSize labelSizeCodec
        |> Codec.field "colour" .colour Style.appColourCodec
        |> Codec.field "channel" .channel Midi.channelCodec
        |> Codec.field "bendValue" .bendValue (Codec.constant 8192)
        |> Codec.buildObject


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

        SizedRow controllers ->
            if currentId == id then
                Just control

            else
                List.indexedMap
                    (\i ( _, c ) ->
                        getWithId (currentId ++ "_" ++ String.fromInt i) id c
                    )
                    controllers
                    |> List.filterMap identity
                    |> List.head

        SizedColumn controllers ->
            if currentId == id then
                Just control

            else
                List.indexedMap
                    (\i ( _, c ) ->
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

        Chord _ ->
            if currentId == id then
                Just control

            else
                Nothing

        CCValue _ ->
            if currentId == id then
                Just control

            else
                Nothing

        Command _ ->
            if currentId == id then
                Just control

            else
                Nothing

        Sequence _ ->
            if currentId == id then
                Just control

            else
                Nothing

        Fader _ ->
            if currentId == id then
                Just control

            else
                Nothing

        XYFader _ ->
            if currentId == id then
                Just control

            else
                Nothing

        PitchBend _ ->
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

        SizedRow controllers ->
            if currentId == id then
                updateFn toUpdate

            else
                List.indexedMap
                    (\i ( s, c ) ->
                        ( s, updateWithId (currentId ++ "_" ++ String.fromInt i) c updateInfo )
                    )
                    controllers
                    |> SizedRow

        SizedColumn controllers ->
            if currentId == id then
                updateFn toUpdate

            else
                List.indexedMap
                    (\i ( s, c ) ->
                        ( s, updateWithId (currentId ++ "_" ++ String.fromInt i) c updateInfo )
                    )
                    controllers
                    |> SizedColumn

        Note state ->
            if currentId == id then
                updateFn toUpdate

            else
                Note state

        Chord state ->
            if currentId == id then
                updateFn toUpdate

            else
                Chord state

        CCValue state ->
            if currentId == id then
                updateFn toUpdate

            else
                CCValue state

        Command state ->
            if currentId == id then
                updateFn toUpdate

            else
                Command state

        Sequence state ->
            if currentId == id then
                updateFn toUpdate

            else
                Sequence state

        Fader state ->
            if currentId == id then
                updateFn toUpdate

            else
                Fader state

        XYFader state ->
            if currentId == id then
                updateFn toUpdate

            else
                XYFader state

        PitchBend state ->
            if currentId == id then
                updateFn toUpdate

            else
                PitchBend state

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


getModules : List Controller -> Controller -> List Controller
getModules currentModules controller =
    case controller of
        (Module _ subController) as modu ->
            (modu :: getModules [] subController)
                |> List.append currentModules

        Row subControls ->
            List.map (getModules []) subControls
                |> List.concat
                |> List.append currentModules

        Column subControls ->
            List.map (getModules []) subControls
                |> List.concat
                |> List.append currentModules

        SizedRow subControls ->
            subControls
                |> List.map Tuple.second
                |> List.map (getModules [])
                |> List.concat
                |> List.append currentModules

        SizedColumn subControls ->
            subControls
                |> List.map Tuple.second
                |> List.map (getModules [])
                |> List.concat
                |> List.append currentModules

        _ ->
            []


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


newNote : String -> LabelSize -> AppColour -> Channel -> Int -> Int -> Controller
newNote label labelSize colour channel pitch velocity =
    Note
        { status = Off
        , label = label
        , labelSize = Just labelSize
        , colour = colour
        , channel = channel
        , pitch = pitch
        , velocity = velocity
        }


newChord :
    String
    -> LabelSize
    -> AppColour
    -> Int
    -> List { channel : Channel, pitch : Int }
    -> Controller
newChord label labelSize colour velocity notes =
    Chord
        { status = Off
        , label = label
        , labelSize = Just labelSize
        , colour = colour
        , velocity = velocity
        , notes = notes
        }


newCCValue : String -> LabelSize -> AppColour -> Channel -> Int -> Int -> Controller
newCCValue label labelSize colour channel controller value =
    CCValue
        { status = Off
        , colour = colour
        , label = label
        , labelSize = Just labelSize
        , channel = channel
        , controller = controller
        , value = value
        }


newCommand : String -> LabelSize -> AppColour -> List MidiMsg -> List MidiMsg -> Controller
newCommand label labelSize colour onPressMsgs onReleaseMsgs =
    Command
        { status = Off
        , label = label
        , labelSize = Just labelSize
        , colour = colour
        , onPressMsgs = onPressMsgs
        , onReleaseMsgs = onReleaseMsgs
        }


newSequence : String -> LabelSize -> AppColour -> Array MidiMsg -> Controller
newSequence label labelSize colour midiMsgs =
    Sequence
        { status = Off
        , label = label
        , labelSize = Just labelSize
        , colour = colour
        , midiMsgs = midiMsgs
        , index = 0
        }


buttonOn : Controller -> ( Controller, List Midi.MidiMsg )
buttonOn controller =
    let
        makeNoteOn { channel, pitch, velocity } =
            Midi.NoteOn
                { channel = Midi.channelToMidiNumber channel
                , pitch = pitch
                , velocity = velocity
                }
    in
    case controller of
        Note state ->
            ( Note { state | status = On }
            , makeNoteOn state
                |> List.singleton
            )

        Chord state ->
            ( Chord { state | status = On }
            , state.notes
                |> List.map
                    (\{ channel, pitch } ->
                        { channel = channel
                        , pitch = pitch
                        , velocity = state.velocity
                        }
                    )
                |> List.map makeNoteOn
            )

        CCValue state ->
            ( CCValue { state | status = On }
            , Midi.ControllerChange
                { channel = Midi.channelToMidiNumber state.channel
                , controller = state.controller
                , value = state.value
                }
                |> List.singleton
            )

        Command state ->
            ( Command { state | status = On }
            , state.onPressMsgs
            )

        Sequence state ->
            ( Sequence
                { state
                    | status = On
                    , index = modBy (Array.length state.midiMsgs) (state.index + 1)
                }
            , Array.get state.index state.midiMsgs
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
            )

        _ ->
            ( controller, [] )


buttonOff : Controller -> ( Controller, List Midi.MidiMsg )
buttonOff controller =
    let
        makeNoteOff { channel, pitch, velocity } =
            Midi.NoteOff
                { channel = Midi.channelToMidiNumber channel
                , pitch = pitch
                , velocity = velocity
                }
    in
    case controller of
        Note state ->
            ( Note { state | status = Off }
            , Midi.NoteOff
                { channel = Midi.channelToMidiNumber state.channel
                , pitch = state.pitch
                , velocity = 0
                }
                |> List.singleton
            )

        Chord state ->
            ( Chord { state | status = Off }
            , state.notes
                |> List.map
                    (\{ channel, pitch } ->
                        { channel = channel
                        , pitch = pitch
                        , velocity = state.velocity
                        }
                    )
                |> List.map makeNoteOff
            )

        CCValue state ->
            ( CCValue { state | status = Off }
            , []
            )

        Command state ->
            ( Command { state | status = Off }
            , state.onReleaseMsgs
            )

        Sequence state ->
            ( Sequence
                { state
                    | status = Off
                }
            , []
            )

        _ ->
            ( controller, [] )


setChannel : Midi.Channel -> Controller -> Controller
setChannel channel controller =
    case controller of
        Module label subControl ->
            Module label (setChannel channel subControl)

        Row subcontrols ->
            Row <|
                List.map (setChannel channel) subcontrols

        Column subcontrols ->
            Column <|
                List.map (setChannel channel) subcontrols

        SizedRow subcontrols ->
            SizedRow <|
                List.map (\( s, c ) -> ( s, setChannel channel c )) subcontrols

        SizedColumn subcontrols ->
            SizedColumn <|
                List.map (\( s, c ) -> ( s, setChannel channel c )) subcontrols

        Note state ->
            Note { state | channel = channel }

        Chord state ->
            Chord
                { state
                    | notes =
                        state.notes
                            |> List.map (\n -> { channel = channel, pitch = n.pitch })
                }

        CCValue state ->
            CCValue { state | channel = channel }

        Command state ->
            Command
                { state
                    | onPressMsgs =
                        List.map
                            (Midi.changeChannel
                                (Midi.channelToMidiNumber channel)
                            )
                            state.onPressMsgs
                    , onReleaseMsgs =
                        List.map
                            (Midi.changeChannel
                                (Midi.channelToMidiNumber channel)
                            )
                            state.onReleaseMsgs
                }

        Sequence state ->
            Sequence
                { state
                    | midiMsgs =
                        Array.map
                            (Midi.changeChannel
                                (Midi.channelToMidiNumber channel)
                            )
                            state.midiMsgs
                }

        Fader state ->
            Fader { state | channel = channel }

        XYFader state ->
            XYFader { state | channel1 = channel, channel2 = channel }

        PitchBend state ->
            PitchBend { state | channel = channel }

        MidiLog ->
            MidiLog

        Space ->
            Space



-- {{ Views


renderController :
    { addSpaceMsg : String -> msg
    , buttonDownMsg : String -> msg
    , buttonUpMsg : String -> msg
    , editMsg : String -> msg
    , faderChangingMsg : String -> Touch.Event -> msg
    , faderChangingMouseMsg : String -> Mouse.Event -> msg
    , faderSetMsg : String -> msg
    , noopMsg : msg
    , removeItemMsg : String -> msg
    }
    -> Bool
    -> List String
    -> PageConfig
    -> List String
    -> Controller
    -> Int
    -> Element msg
renderController msgs editing midiLog config idParts controller id =
    let
        updatedParts =
            String.fromInt id :: idParts

        parentId =
            updatedParts
                |> List.reverse
                |> String.join "_"
    in
    case controller of
        Module label subControls ->
            Lazy.lazy2
                column
                ([ padding config.gapSize
                 , spacing config.gapSize
                 ]
                    ++ fillSpace
                    ++ (if not editing then
                            [ Border.dotted
                            , Border.width 4
                            ]

                        else
                            [ padding 2
                            , Border.width 2
                            , Border.dashed
                            ]
                       )
                )
                [ row [ width fill ]
                    ((el [ padding 4 ] <| text label)
                        :: (if not editing then
                                []

                            else
                                [ el
                                    [ alignRight
                                    , padding 4
                                    , backgroundColour White
                                    ]
                                  <|
                                    renderEditButton
                                        { editMsg =
                                            msgs.editMsg parentId
                                        , addMsg = msgs.addSpaceMsg parentId
                                        , removeMsg = msgs.removeItemMsg parentId
                                        }
                                        config
                                        EditContainer
                                ]
                           )
                    )
                , renderController msgs editing midiLog config updatedParts subControls 0
                ]

        Row subControls ->
            Lazy.lazy2 row
                ([ spacingXY config.gapSize 0
                 , Events.onClick msgs.noopMsg
                 ]
                    ++ fillSpace
                    ++ (if not editing then
                            []

                        else
                            [ padding 2
                            , Border.width 2
                            , Border.dashed
                            ]
                       )
                )
            <|
                (List.map2
                    (renderController msgs editing midiLog config updatedParts)
                    subControls
                    (List.range 0 <| List.length subControls)
                    ++ (if not editing then
                            []

                        else
                            [ el
                                [ alignRight
                                , padding 4
                                , backgroundColour White
                                ]
                              <|
                                renderEditButton
                                    { editMsg =
                                        msgs.editMsg parentId
                                    , addMsg = msgs.addSpaceMsg parentId
                                    , removeMsg = msgs.removeItemMsg parentId
                                    }
                                    config
                                    EditContainer
                            ]
                       )
                )

        Column subControls ->
            if not editing then
                Lazy.lazy2 column
                    (spacingXY 0 config.gapSize
                        :: fillSpace
                        ++ (if not editing then
                                []

                            else
                                [ padding 2
                                , Border.width 2
                                , Border.dashed
                                ]
                           )
                    )
                <|
                    List.map2
                        (renderController msgs editing midiLog config updatedParts)
                        subControls
                        (List.range 0 <| List.length subControls)

            else
                Lazy.lazy2 column
                    ([ paddingXY 0 5
                     , spacing 5
                     , Border.width 2
                     , Border.dashed
                     ]
                        ++ fillSpace
                    )
                    [ renderEditButton
                        { editMsg =
                            msgs.editMsg parentId
                        , addMsg = msgs.addSpaceMsg parentId
                        , removeMsg = msgs.removeItemMsg parentId
                        }
                        config
                        EditContainer
                    , column
                        ([ spacingXY 0 config.gapSize
                         , padding config.gapSize
                         ]
                            ++ fillSpace
                        )
                      <|
                        List.map2
                            (renderController msgs editing midiLog config updatedParts)
                            subControls
                            (List.range 0 <| List.length subControls)
                    ]

        SizedRow subControls ->
            Lazy.lazy2 row
                ([ spacingXY config.gapSize 0
                 , Events.onClick msgs.noopMsg
                 ]
                    ++ fillSpace
                    ++ (if not editing then
                            []

                        else
                            [ padding 2
                            , Border.width 2
                            , Border.dashed
                            ]
                       )
                )
            <|
                (List.map2
                    (\( s, c ) i ->
                        el [ height fill, width <| fillPortion s ]
                            (renderController msgs editing midiLog config updatedParts c i)
                    )
                    subControls
                    (List.range 0 <| List.length subControls)
                    ++ (if not editing then
                            []

                        else
                            [ el
                                [ alignRight
                                , padding 4
                                , backgroundColour White
                                ]
                              <|
                                renderEditButton
                                    { editMsg =
                                        msgs.editMsg parentId
                                    , addMsg = msgs.addSpaceMsg parentId
                                    , removeMsg = msgs.removeItemMsg parentId
                                    }
                                    config
                                    EditContainer
                            ]
                       )
                )

        SizedColumn subControls ->
            if not editing then
                Lazy.lazy2 column
                    (spacingXY 0 config.gapSize
                        :: fillSpace
                        ++ (if not editing then
                                []

                            else
                                [ padding 2
                                , Border.width 2
                                , Border.dashed
                                ]
                           )
                    )
                <|
                    List.map2
                        (\( s, c ) i ->
                            el [ height <| fillPortion s, width fill ]
                                (renderController msgs editing midiLog config updatedParts c i)
                        )
                        subControls
                        (List.range 0 <| List.length subControls)

            else
                Lazy.lazy2 column
                    ([ paddingXY 0 5
                     , spacing 5
                     , Border.width 2
                     , Border.dashed
                     ]
                        ++ fillSpace
                    )
                    [ renderEditButton
                        { editMsg =
                            msgs.editMsg parentId
                        , addMsg = msgs.addSpaceMsg parentId
                        , removeMsg = msgs.removeItemMsg parentId
                        }
                        config
                        EditContainer
                    , column
                        ([ spacingXY 0 config.gapSize
                         , padding config.gapSize
                         ]
                            ++ fillSpace
                        )
                      <|
                        List.map2
                            (\( s, c ) i ->
                                el [ height <| fillPortion s, width fill ]
                                    (renderController msgs editing midiLog config updatedParts c i)
                            )
                            subControls
                            (List.range 0 <| List.length subControls)
                    ]

        Note state ->
            renderNote
                { buttonDownMsg = msgs.buttonDownMsg parentId
                , buttonUpMsg = msgs.buttonUpMsg parentId
                , editMsg = msgs.editMsg parentId
                }
                config
                editing
                state

        Chord state ->
            renderChord
                { buttonDownMsg = msgs.buttonDownMsg parentId
                , buttonUpMsg = msgs.buttonUpMsg parentId
                , editMsg = msgs.editMsg parentId
                }
                config
                editing
                state

        CCValue state ->
            renderCCValue
                { buttonDownMsg = msgs.buttonDownMsg parentId
                , buttonUpMsg = msgs.buttonUpMsg parentId
                , editMsg = msgs.editMsg parentId
                }
                config
                editing
                state

        Command state ->
            renderCommand
                { buttonDownMsg = msgs.buttonDownMsg parentId
                , buttonUpMsg = msgs.buttonUpMsg parentId
                , editMsg = msgs.editMsg parentId
                }
                config
                editing
                state

        Sequence state ->
            renderSequence
                { buttonDownMsg = msgs.buttonDownMsg parentId
                , buttonUpMsg = msgs.buttonUpMsg parentId
                , editMsg = msgs.editMsg parentId
                }
                config
                editing
                state

        Fader state ->
            renderFader
                { faderChangingMsg = msgs.faderChangingMsg parentId
                , faderChangingMouseMsg = msgs.faderChangingMouseMsg parentId
                , faderSetMsg = msgs.faderSetMsg parentId
                , editMsg = msgs.editMsg parentId
                }
                config
                editing
                state

        XYFader state ->
            renderXYFader
                { faderChangingMsg = msgs.faderChangingMsg parentId
                , faderChangingMouseMsg = msgs.faderChangingMouseMsg parentId
                , faderSetMsg = msgs.faderSetMsg parentId
                , editMsg = msgs.editMsg parentId
                }
                config
                editing
                state

        PitchBend state ->
            renderPitchBend
                { faderChangingMsg = msgs.faderChangingMsg parentId
                , faderChangingMouseMsg = msgs.faderChangingMouseMsg parentId
                , faderSetMsg = msgs.faderSetMsg parentId
                , editMsg = msgs.editMsg parentId
                }
                config
                editing
                state

        MidiLog ->
            if not editing then
                column fillSpace
                    [ column
                        ([ padding 4
                         , spacing 10
                         , backgroundColour White
                         , scrollbarY
                         , clipX
                         ]
                            ++ fillSpace
                        )
                        (if List.isEmpty midiLog then
                            [ el [ centerX, centerY ] <|
                                text "No MIDI events in log."
                            ]

                         else
                            List.map
                                (\s ->
                                    paragraph
                                        [ Font.alignLeft
                                        , Font.size 12
                                        , padding 2
                                        , Border.widthEach
                                            { bottom = 1, top = 0, left = 0, right = 0 }
                                        ]
                                        [ text s ]
                                )
                                midiLog
                        )
                    ]

            else
                el
                    ([ backgroundColour LightGrey
                     , Events.onClick <|
                        msgs.editMsg parentId
                     ]
                        ++ fillSpace
                    )
                    (el
                        [ centerX
                        , centerY
                        ]
                     <|
                        text "MIDI Log"
                    )

        Space ->
            if not editing then
                el
                    ([ backgroundColour White
                     , borderColour LightGrey
                     , Border.width 4
                     ]
                        ++ fillSpace
                    )
                    none

            else
                el
                    ([ backgroundColour LightGrey
                     , Events.onClick <|
                        msgs.editMsg parentId
                     ]
                        ++ fillSpace
                    )
                    (el
                        [ centerX
                        , centerY
                        ]
                     <|
                        text "SPACE"
                    )


renderNote :
    { buttonDownMsg : msg, buttonUpMsg : msg, editMsg : msg }
    -> PageConfig
    -> Bool
    -> NoteState
    -> Element msg
renderNote msgs config editing state =
    if not editing then
        el
            ([ padding 0
             , spacing 0
             , Border.width 4
             , case state.status of
                Off ->
                    backgroundColour state.colour

                On ->
                    Border.dashed
             , htmlAttribute <|
                Touch.onStart
                    (\_ ->
                        msgs.buttonDownMsg
                    )
             , htmlAttribute <|
                Mouse.onDown
                    (\event ->
                        msgs.buttonDownMsg
                    )
             , htmlAttribute <|
                Touch.onEnd
                    (\_ ->
                        msgs.buttonUpMsg
                    )
             , htmlAttribute <|
                Mouse.onUp
                    (\_ ->
                        msgs.buttonUpMsg
                    )
             ]
                ++ Style.noSelect
                ++ fillSpace
            )
            ((if config.debug then
                Midi.channelToString state.channel ++ "\n"

              else
                ""
             )
                ++ state.label
                |> text
                |> el
                    [ centerX
                    , centerY
                    , labelSizeToFontSize <| Maybe.withDefault Small state.labelSize
                    ]
            )

    else
        Input.button
            ([ padding config.gapSize
             , spacing config.gapSize
             , Border.width 2
             , Border.dashed
             , Font.size 14
             , backgroundColour state.colour
             ]
                ++ Style.noSelect
                ++ fillSpace
            )
            { onPress = Just <| msgs.editMsg
            , label =
                state.label
                    |> text
            }


renderChord :
    { buttonDownMsg : msg, buttonUpMsg : msg, editMsg : msg }
    -> PageConfig
    -> Bool
    -> ChordState
    -> Element msg
renderChord msgs config editing state =
    if not editing then
        el
            ([ padding 0
             , spacing 0
             , Border.width 4
             , case state.status of
                Off ->
                    backgroundColour state.colour

                On ->
                    Border.dashed
             , htmlAttribute <|
                Touch.onStart
                    (\_ ->
                        msgs.buttonDownMsg
                    )
             , htmlAttribute <|
                Mouse.onDown
                    (\_ ->
                        msgs.buttonDownMsg
                    )
             , htmlAttribute <|
                Touch.onEnd
                    (\_ ->
                        msgs.buttonUpMsg
                    )
             , htmlAttribute <|
                Mouse.onUp
                    (\_ ->
                        msgs.buttonUpMsg
                    )
             ]
                ++ Style.noSelect
                ++ fillSpace
            )
            (state.label
                |> text
                |> el
                    [ centerX
                    , centerY
                    , labelSizeToFontSize <| Maybe.withDefault Small state.labelSize
                    ]
            )

    else
        Input.button
            ([ padding config.gapSize
             , spacing config.gapSize
             , Border.width 2
             , Border.dashed
             , Font.size 14
             , backgroundColour state.colour
             ]
                ++ Style.noSelect
                ++ fillSpace
            )
            { onPress = Just <| msgs.editMsg
            , label =
                state.label
                    |> text
            }


renderCCValue :
    { buttonDownMsg : msg, buttonUpMsg : msg, editMsg : msg }
    -> PageConfig
    -> Bool
    -> CCValueState
    -> Element msg
renderCCValue msgs config editing state =
    if not editing then
        el
            ([ padding 0
             , spacing 0
             , Border.width 4
             , case state.status of
                Off ->
                    backgroundColour state.colour

                On ->
                    Border.dashed
             , htmlAttribute <|
                Touch.onStart
                    (\_ ->
                        msgs.buttonDownMsg
                    )
             , htmlAttribute <|
                Mouse.onDown
                    (\_ ->
                        msgs.buttonDownMsg
                    )
             , htmlAttribute <|
                Touch.onEnd
                    (\_ ->
                        msgs.buttonUpMsg
                    )
             , htmlAttribute <|
                Mouse.onUp
                    (\_ ->
                        msgs.buttonUpMsg
                    )
             ]
                ++ Style.noSelect
                ++ fillSpace
            )
            ((if config.debug then
                case state.status of
                    Off ->
                        "Off\n" ++ Midi.channelToString state.channel ++ "\n"

                    On ->
                        "Off\n" ++ Midi.channelToString state.channel ++ "\n"

              else
                ""
             )
                ++ state.label
                |> text
                |> el
                    [ centerX
                    , centerY
                    , labelSizeToFontSize <| Maybe.withDefault Small state.labelSize
                    ]
            )

    else
        Input.button
            ([ padding config.gapSize
             , spacing config.gapSize
             , Border.width 2
             , Border.dashed
             , Font.size 14
             , backgroundColour state.colour
             ]
                ++ Style.noSelect
                ++ fillSpace
            )
            { onPress = Just <| msgs.editMsg
            , label =
                state.label
                    |> text
            }


renderCommand :
    { buttonDownMsg : msg, buttonUpMsg : msg, editMsg : msg }
    -> PageConfig
    -> Bool
    -> CommandState
    -> Element msg
renderCommand msgs config editing state =
    if not editing then
        el
            ([ padding 0
             , spacing 0
             , Border.width 4
             , case state.status of
                Off ->
                    backgroundColour state.colour

                On ->
                    Border.dashed
             , htmlAttribute <|
                Touch.onStart
                    (\_ ->
                        msgs.buttonDownMsg
                    )
             , htmlAttribute <|
                Mouse.onDown
                    (\_ ->
                        msgs.buttonDownMsg
                    )
             , htmlAttribute <|
                Touch.onEnd
                    (\_ ->
                        msgs.buttonUpMsg
                    )
             , htmlAttribute <|
                Mouse.onUp
                    (\_ ->
                        msgs.buttonUpMsg
                    )
             ]
                ++ Style.noSelect
                ++ fillSpace
            )
            (state.label
                |> text
                |> el
                    [ centerX
                    , centerY
                    , labelSizeToFontSize <| Maybe.withDefault Small state.labelSize
                    ]
            )

    else
        Input.button
            ([ padding config.gapSize
             , spacing config.gapSize
             , Border.width 2
             , Border.dashed
             , Font.size 14
             , backgroundColour state.colour
             ]
                ++ Style.noSelect
                ++ fillSpace
            )
            { onPress = Just <| msgs.editMsg
            , label =
                state.label
                    |> text
            }


renderSequence :
    { buttonDownMsg : msg, buttonUpMsg : msg, editMsg : msg }
    -> PageConfig
    -> Bool
    -> SequenceState
    -> Element msg
renderSequence msgs config editing state =
    if not editing then
        el
            ([ padding 0
             , spacing 0
             , Border.width 4
             , case state.status of
                Off ->
                    backgroundColour state.colour

                On ->
                    Border.dashed
             , htmlAttribute <|
                Touch.onStart
                    (\_ ->
                        msgs.buttonDownMsg
                    )
             , htmlAttribute <|
                Mouse.onDown
                    (\_ ->
                        msgs.buttonDownMsg
                    )
             , htmlAttribute <|
                Touch.onEnd
                    (\_ ->
                        msgs.buttonUpMsg
                    )
             , htmlAttribute <|
                Mouse.onUp
                    (\_ ->
                        msgs.buttonUpMsg
                    )
             ]
                ++ Style.noSelect
                ++ fillSpace
            )
            (state.label
                ++ "\n"
                ++ String.fromInt (state.index + 1)
                |> text
                |> el
                    [ centerX
                    , centerY
                    , labelSizeToFontSize <| Maybe.withDefault Small state.labelSize
                    ]
            )

    else
        Input.button
            ([ padding config.gapSize
             , spacing config.gapSize
             , Border.width 2
             , Border.dashed
             , Font.size 14
             , backgroundColour state.colour
             ]
                ++ Style.noSelect
                ++ fillSpace
            )
            { onPress = Just <| msgs.editMsg
            , label =
                state.label
                    |> text
            }


renderFader :
    { faderChangingMsg : Touch.Event -> msg, faderChangingMouseMsg : Mouse.Event -> msg, faderSetMsg : msg, editMsg : msg }
    -> PageConfig
    -> Bool
    -> FaderState
    -> Element msg
renderFader msgs config editing state =
    if not editing then
        el
            ([ padding 0
             , spacing 0
             , Border.width 4
             , case state.status of
                Set ->
                    Border.solid

                Changing _ _ ->
                    Border.dashed
             , htmlAttribute <|
                Touch.onStart
                    (\event ->
                        msgs.faderChangingMsg event
                    )
             , htmlAttribute <|
                Mouse.onDown
                    (\event ->
                        msgs.faderChangingMouseMsg event
                    )
             , htmlAttribute <|
                Touch.onMove
                    (\event ->
                        msgs.faderChangingMsg event
                    )
             , htmlAttribute <|
                Touch.onEnd
                    (\_ ->
                        msgs.faderSetMsg
                    )
             , htmlAttribute <|
                Mouse.onUp
                    (\_ ->
                        msgs.faderSetMsg
                    )
             ]
                ++ (case state.status of
                        Changing _ _ ->
                            [ htmlAttribute <|
                                Mouse.onMove
                                    (\event ->
                                        msgs.faderChangingMouseMsg event
                                    )
                            ]

                        Set ->
                            []
                   )
                ++ Style.noSelect
                ++ fillSpace
            )
            (column
                (backgroundColour White :: fillSpace)
                [ el [ centerX, padding 10, Font.size 14 ] <|
                    text (String.fromInt state.valuePercent ++ "%")
                , column
                    fillSpace
                    [ el
                        [ height <| fillPortion (100 - state.valuePercent)
                        , width fill
                        , backgroundColour White
                        ]
                        none
                    , el
                        [ height <| fillPortion state.valuePercent
                        , width fill
                        , backgroundColour state.colour
                        , Border.widthEach { bottom = 0, top = 8, left = 0, right = 0 }
                        ]
                        none
                    ]
                , el
                    [ centerX
                    , padding 10
                    , labelSizeToFontSize <| Maybe.withDefault Small state.labelSize
                    ]
                  <|
                    text state.label
                ]
            )

    else
        Input.button
            ([ padding config.gapSize
             , spacing config.gapSize
             , Border.width 2
             , Border.dashed
             , Font.size 14
             , backgroundColour state.colour
             ]
                ++ Style.noSelect
                ++ fillSpace
            )
            { onPress = Just <| msgs.editMsg
            , label =
                state.label
                    |> text
            }


renderXYFader :
    { faderChangingMsg : Touch.Event -> msg, faderChangingMouseMsg : Mouse.Event -> msg, faderSetMsg : msg, editMsg : msg }
    -> PageConfig
    -> Bool
    -> XYFaderState
    -> Element msg
renderXYFader msgs config editing state =
    if not editing then
        el
            ([ padding 0
             , spacing 0
             , Border.width 4
             , case state.status of
                Set ->
                    Border.solid

                Changing _ _ ->
                    Border.dashed
             , htmlAttribute <|
                Touch.onStart
                    (\event ->
                        msgs.faderChangingMsg event
                    )
             , htmlAttribute <|
                Mouse.onDown
                    (\event ->
                        msgs.faderChangingMouseMsg event
                    )
             , htmlAttribute <|
                Touch.onMove
                    (\event ->
                        msgs.faderChangingMsg event
                    )
             , htmlAttribute <|
                Touch.onEnd
                    (\_ ->
                        msgs.faderSetMsg
                    )
             , htmlAttribute <|
                Mouse.onUp
                    (\_ ->
                        msgs.faderSetMsg
                    )
             ]
                ++ (case state.status of
                        Changing _ _ ->
                            [ htmlAttribute <|
                                Mouse.onMove
                                    (\event ->
                                        msgs.faderChangingMouseMsg event
                                    )
                            ]

                        Set ->
                            []
                   )
                ++ Style.noSelect
                ++ fillSpace
            )
            (column
                (backgroundColour White :: fillSpace)
                [ el [ centerX, padding 10, Font.size 14 ] <|
                    text
                        ("X "
                            ++ String.fromInt state.valuePercent1
                            ++ "%, "
                            ++ "Y "
                            ++ String.fromInt state.valuePercent2
                            ++ "%"
                        )
                , column
                    ([ inFront <|
                        row
                            fillSpace
                            [ el
                                [ height fill
                                , width <| fillPortion state.valuePercent1
                                ]
                                none
                            , el
                                [ height fill
                                , width <| fillPortion (100 - state.valuePercent1)
                                , Border.widthEach { bottom = 0, top = 0, left = 8, right = 0 }
                                ]
                                none
                            ]
                     , backgroundColour state.colour
                     ]
                        ++ fillSpace
                    )
                    [ el
                        [ height <| fillPortion (100 - state.valuePercent2)
                        , width fill
                        ]
                        none
                    , el
                        [ height <| fillPortion state.valuePercent2
                        , width fill
                        , Border.widthEach { bottom = 0, top = 8, left = 0, right = 0 }
                        ]
                        none
                    ]
                , el
                    [ centerX
                    , padding 10
                    , labelSizeToFontSize <| Maybe.withDefault Small state.labelSize
                    ]
                  <|
                    text state.label
                ]
            )

    else
        Input.button
            ([ padding config.gapSize
             , spacing config.gapSize
             , Border.width 2
             , Border.dashed
             , Font.size 14
             , backgroundColour state.colour
             ]
                ++ Style.noSelect
                ++ fillSpace
            )
            { onPress = Just <| msgs.editMsg
            , label =
                state.label
                    |> text
            }


renderPitchBend :
    { faderChangingMsg : Touch.Event -> msg, faderChangingMouseMsg : Mouse.Event -> msg, faderSetMsg : msg, editMsg : msg }
    -> PageConfig
    -> Bool
    -> PitchBendState
    -> Element msg
renderPitchBend msgs config editing state =
    let
        fillFraction =
            round <| 100 * (toFloat state.bendValue / 16383)
    in
    if not editing then
        el
            ([ padding 0
             , spacing 0
             , Border.width 4
             , case state.status of
                Set ->
                    Border.solid

                Changing _ _ ->
                    Border.dashed
             , htmlAttribute <|
                Touch.onStart
                    (\event ->
                        msgs.faderChangingMsg event
                    )
             , htmlAttribute <|
                Mouse.onDown
                    (\event ->
                        msgs.faderChangingMouseMsg event
                    )
             , htmlAttribute <|
                Touch.onMove
                    (\event ->
                        msgs.faderChangingMsg event
                    )
             , htmlAttribute <|
                Touch.onEnd
                    (\_ ->
                        msgs.faderSetMsg
                    )
             , htmlAttribute <|
                Mouse.onUp
                    (\_ ->
                        msgs.faderSetMsg
                    )
             ]
                ++ (case state.status of
                        Changing _ _ ->
                            [ htmlAttribute <|
                                Mouse.onMove
                                    (\event ->
                                        msgs.faderChangingMouseMsg event
                                    )
                            ]

                        Set ->
                            []
                   )
                ++ Style.noSelect
                ++ fillSpace
            )
            (column
                (backgroundColour White :: fillSpace)
                [ column
                    fillSpace
                    [ el
                        [ height <| fillPortion (100 - fillFraction)
                        , width fill
                        , backgroundColour state.colour
                        ]
                        none
                    , el
                        [ height <| fillPortion fillFraction
                        , width fill
                        , backgroundColour state.colour
                        , Border.widthEach { bottom = 0, top = 8, left = 0, right = 0 }
                        ]
                        none
                    ]
                , el
                    [ centerX
                    , padding 10
                    , labelSizeToFontSize <| Maybe.withDefault Small state.labelSize
                    ]
                  <|
                    text state.label
                ]
            )

    else
        Input.button
            ([ padding config.gapSize
             , spacing config.gapSize
             , Border.width 2
             , Border.dashed
             , Font.size 14
             , backgroundColour state.colour
             ]
                ++ Style.noSelect
                ++ fillSpace
            )
            { onPress = Just <| msgs.editMsg
            , label =
                state.label
                    |> text
            }


renderEditButton : { editMsg : msg, addMsg : msg, removeMsg : msg } -> PageConfig -> EditOperation -> Element msg
renderEditButton msgs config editOperation =
    case editOperation of
        EditContainer ->
            Input.button
                [ centerX
                , padding config.gapSize
                , spacing config.gapSize
                , Border.width 2
                ]
                { onPress = Just <| msgs.editMsg
                , label =
                    Icons.edit2
                        |> Icons.withSize 20
                        |> Icons.toHtml []
                        |> html
                }

        Add ->
            Input.button
                [ centerX
                , padding config.gapSize
                , spacing config.gapSize
                , Border.width 2
                ]
                { onPress = Just <| msgs.addMsg
                , label =
                    Icons.plus
                        |> Icons.withSize 20
                        |> Icons.toHtml []
                        |> html
                }

        Remove ->
            Input.button
                [ centerX
                , padding config.gapSize
                , spacing config.gapSize
                , Border.width 2
                ]
                { onPress = Just <| msgs.removeMsg
                , label =
                    Icons.minus
                        |> Icons.withSize 20
                        |> Icons.toHtml []
                        |> html
                }



-- }}
