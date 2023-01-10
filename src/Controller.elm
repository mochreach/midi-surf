module Controller exposing (..)

import Codec exposing (Codec)
import Midi exposing (Channel(..), MidiMsg(..))
import Style exposing (AppColour(..))


type Controller
    = Module String Controller
    | Row (List Controller)
    | Column (List Controller)
    | Note NoteState
    | Chord ChordState
    | CCValue CCValueState
    | Fader FaderState
    | MidiLog
    | Space


controllerCodec : Codec Controller
controllerCodec =
    Codec.recursive
        (\rmeta ->
            Codec.custom
                (\mod row col note cho ccv fad mid spa value ->
                    case value of
                        Module l c ->
                            mod l c

                        Row cs ->
                            row cs

                        Column cs ->
                            col cs

                        Note s ->
                            note s

                        Chord s ->
                            cho s

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
                |> Codec.variant1 "Chord" Chord chordStateCodec
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

        Fader { channel, ccNumber } ->
            "Fader: "
                ++ Midi.channelToString channel
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
        |> Codec.field "channel" .channel Midi.channelCodec
        |> Codec.field "pitch" .pitch Codec.int
        |> Codec.field "velocity" .velocity Codec.int
        |> Codec.buildObject


type alias ChordState =
    { status : ButtonStatus
    , label : String
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
        |> Codec.field "channel" .channel Midi.channelCodec
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
        |> Codec.field "channel" .channel Midi.channelCodec
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
                            { channel = Midi.channelToMidiNumber state.channel
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


newNote : String -> AppColour -> Channel -> Int -> Int -> Controller
newNote label colour channel pitch velocity =
    Note
        { status = Off
        , label = label
        , colour = colour
        , channel = channel
        , pitch = pitch
        , velocity = velocity
        }


newChord :
    String
    -> AppColour
    -> Int
    -> List { channel : Channel, pitch : Int }
    -> Controller
newChord label colour velocity notes =
    Chord
        { status = Off
        , label = label
        , colour = colour
        , velocity = velocity
        , notes = notes
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

        Fader state ->
            Fader { state | channel = channel }

        MidiLog ->
            MidiLog

        Space ->
            Space
