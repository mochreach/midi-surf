module Controller exposing
    ( ButtonState
    , ButtonStatus(..)
    , Channel(..)
    , Controller(..)
    , EditOperation(..)
    , addSpace
    , buttonOff
    , buttonOn
    , channelToMidiNumber
    , channelToString
    , getWithId
    , newButton
    , removeItem
    , stringToChannel
    , updateWithId
    )


type Controller
    = Module String Controller
    | Row (List Controller)
    | Column (List Controller)
    | Button ButtonState
    | Space


type alias ButtonState =
    { status : ButtonStatus
    , label : String
    , channel : Channel
    , noteNumber : Int
    , velocity : Int
    }


type ButtonStatus
    = On
    | Off


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


type EditOperation
    = Add
    | Remove


getWithId : String -> String -> Controller -> Maybe Controller
getWithId currentId id controller =
    case controller of
        Module _ subController ->
            if currentId == id then
                Just controller

            else
                getWithId (currentId ++ "_0") id subController

        Row controllers ->
            if currentId == id then
                Just controller

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
                Just controller

            else
                List.indexedMap
                    (\i c ->
                        getWithId (currentId ++ "_" ++ String.fromInt i) id c
                    )
                    controllers
                    |> List.filterMap identity
                    |> List.head

        Button _ ->
            if currentId == id then
                Just controller

            else
                Nothing

        Space ->
            if currentId == id then
                Just controller

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

        Button state ->
            if currentId == id then
                updateFn toUpdate

            else
                Button state

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


newButton : String -> Channel -> Int -> Int -> Controller
newButton label channel noteNumber velocity =
    Button
        { status = Off
        , label = label
        , channel = channel
        , noteNumber = noteNumber
        , velocity = velocity
        }


buttonOn : Controller -> Controller
buttonOn controller =
    case controller of
        Button state ->
            Button { state | status = On }

        _ ->
            controller


buttonOff : Controller -> Controller
buttonOff controller =
    case controller of
        Button state ->
            Button { state | status = Off }

        _ ->
            controller
