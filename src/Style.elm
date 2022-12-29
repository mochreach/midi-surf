module Style exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


type AppColour
    = White
    | LightGrey
    | DarkGrey
    | Black
    | Green
    | Blue
    | Yellow
    | Red


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
