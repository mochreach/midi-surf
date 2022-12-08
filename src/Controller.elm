module Controller exposing
    ( ButtonState
    , ButtonStatus(..)
    , Controller(..)
    , EditOperation(..)
    , addSpace
    , buttonOff
    , buttonOn
    , getWithId
    , newButton
    , removeItem
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
    , noteNumber : Int
    }


type ButtonStatus
    = On
    | Off


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


newButton : String -> Int -> Controller
newButton label noteNumber =
    Button
        { status = Off
        , label = label
        , noteNumber = noteNumber
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
