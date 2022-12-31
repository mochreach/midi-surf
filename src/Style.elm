module Style exposing (..)

import Codec exposing (Codec)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes as Hatt


type AppColour
    = White
    | LightGrey
    | DarkGrey
    | Black
    | Green
    | Blue
    | Yellow
    | Red


appColourCodec : Codec AppColour
appColourCodec =
    Codec.custom
        (\wh lg dg bk gr bl ye re value ->
            case value of
                White ->
                    wh

                LightGrey ->
                    lg

                DarkGrey ->
                    dg

                Black ->
                    bk

                Green ->
                    gr

                Blue ->
                    bl

                Yellow ->
                    ye

                Red ->
                    re
        )
        |> Codec.variant0 "White" White
        |> Codec.variant0 "LightGrey" LightGrey
        |> Codec.variant0 "DarkGrey" DarkGrey
        |> Codec.variant0 "Black" Black
        |> Codec.variant0 "Green" Green
        |> Codec.variant0 "Blue" Blue
        |> Codec.variant0 "Yellow" Yellow
        |> Codec.variant0 "Red" Red
        |> Codec.buildCustom


appColourToRGB : AppColour -> Color
appColourToRGB appColour =
    case appColour of
        White ->
            rgb255 255 255 255

        LightGrey ->
            rgb255 200 200 200

        DarkGrey ->
            rgb255 86 90 94

        Black ->
            rgb255 0 0 0

        Green ->
            rgb255 0 133 86

        Blue ->
            rgb255 35 141 193

        Yellow ->
            rgb255 255 183 27

        Red ->
            rgb255 202 0 61


pitchToAppColour : Int -> AppColour
pitchToAppColour pitch =
    if modBy 12 pitch == 0 then
        Blue

    else if List.member (modBy 12 pitch) [ 1, 3, 6, 8, 10 ] then
        DarkGrey

    else
        LightGrey


backgroundColour : AppColour -> Element.Attribute msg
backgroundColour appColour =
    appColourToRGB appColour
        |> Background.color


borderColour : AppColour -> Element.Attribute msg
borderColour appColour =
    appColourToRGB appColour
        |> Border.color


fontColour : AppColour -> Element.Attribute msg
fontColour appColour =
    appColourToRGB appColour
        |> Font.color


noSelect : List (Element.Attribute msg)
noSelect =
    List.map htmlAttribute
        [ Hatt.style "-webkit-touch-callout" "none"
        , Hatt.style "-webkit-user-select" "none"
        , Hatt.style "-khtml-user-select" "none"
        , Hatt.style "-moz-user-select" "none"
        , Hatt.style "-ms-user-select" "none"
        , Hatt.style "user-select" "none"
        ]


fillSpace : List (Attribute msg)
fillSpace =
    [ height fill, width fill ]
