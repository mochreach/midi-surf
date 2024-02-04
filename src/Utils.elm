module Utils exposing (PageConfig, mmap6, onContextMenu, pageConfigCodec)

import Codec exposing (Codec, Value)
import Html
import Html.Events exposing (preventDefaultOn)
import Json.Decode as Json


type alias PageConfig =
    { gapSize : Int
    , debug : Bool
    }


pageConfigCodec : Codec PageConfig
pageConfigCodec =
    Codec.object PageConfig
        |> Codec.field "gapSize" .gapSize Codec.int
        |> Codec.field "debug" .debug Codec.bool
        |> Codec.buildObject


onContextMenu : msg -> Html.Attribute msg
onContextMenu msg =
    preventDefaultOn "contextmenu" (Json.map alwaysPreventDefault (Json.succeed msg))


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


mmap6 :
    (a -> b -> c -> d -> e -> f -> value)
    -> Maybe a
    -> Maybe b
    -> Maybe c
    -> Maybe d
    -> Maybe e
    -> Maybe f
    -> Maybe value
mmap6 func ma mb mc md me mf =
    case ma of
        Nothing ->
            Nothing

        Just a ->
            case mb of
                Nothing ->
                    Nothing

                Just b ->
                    case mc of
                        Nothing ->
                            Nothing

                        Just c ->
                            case md of
                                Nothing ->
                                    Nothing

                                Just d ->
                                    case me of
                                        Nothing ->
                                            Nothing

                                        Just e ->
                                            case mf of
                                                Nothing ->
                                                    Nothing

                                                Just f ->
                                                    Just (func a b c d e f)
