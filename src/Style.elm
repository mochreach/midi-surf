module Style exposing (..)

import Codec exposing (Codec)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as HAtt


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


colourRadio : AppColour -> (AppColour -> msg) -> Element msg
colourRadio colour msg =
    Input.radio
        [ padding 2
        , spacing 10
        , height (px 100)
        , width fill
        , scrollbarY
        , Border.width 2
        , Border.dashed
        ]
        { onChange = msg
        , selected = Just colour
        , label =
            Input.labelAbove
                [ paddingEach { top = 0, bottom = 10, left = 0, right = 0 }
                ]
                (text "Colour")
        , options =
            [ Input.option Green (text "Green")
            , Input.option Blue (text "Blue")
            , Input.option Yellow (text "Yellow")
            , Input.option Red (text "Red")
            , Input.option White (text "White")
            , Input.option LightGrey (text "Light Grey")
            , Input.option DarkGrey (text "Dark Grey")
            , Input.option Black (text "Black")
            ]
        }


type LabelSize
    = Small
    | Medium
    | Large
    | ExtraLarge


labelSizeCodec : Codec LabelSize
labelSizeCodec =
    Codec.custom
        (\sm me la xl value ->
            case value of
                Small ->
                    sm

                Medium ->
                    me

                Large ->
                    la

                ExtraLarge ->
                    xl
        )
        |> Codec.variant0 "Small" Small
        |> Codec.variant0 "Medium" Medium
        |> Codec.variant0 "Large" Large
        |> Codec.variant0 "ExtraLarge" ExtraLarge
        |> Codec.buildCustom


labelSizeToFontSize : LabelSize -> Element.Attribute msg
labelSizeToFontSize labelSize =
    case labelSize of
        Small ->
            Font.size 14

        Medium ->
            Font.size 24

        Large ->
            Font.size 32

        ExtraLarge ->
            Font.size 48


labelSizeRadio : LabelSize -> (LabelSize -> msg) -> Element msg
labelSizeRadio labelSize msg =
    Input.radio
        [ padding 2
        , spacing 10
        , height (px 100)
        , width fill
        , scrollbarY
        , Border.width 2
        , Border.dashed
        ]
        { onChange = msg
        , selected = Just labelSize
        , label =
            Input.labelAbove
                [ paddingEach { top = 0, bottom = 10, left = 0, right = 0 }
                ]
                (text "Label size")
        , options =
            [ Input.option Small (text "Small")
            , Input.option Medium (text "Medium")
            , Input.option Large (text "Large")
            , Input.option ExtraLarge (text "ExtraLarge")
            ]
        }


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


acceptOrCloseButtons : String -> msg -> Maybe msg -> Element msg
acceptOrCloseButtons acceptString cancelMsg acceptMsg =
    row
        [ alignTop
        , spacing 4
        , backgroundColour White
        ]
        [ Input.button
            ([ padding 5
             , Border.width 2
             , Border.solid
             , borderColour Black
             ]
                ++ (case acceptMsg of
                        Just _ ->
                            [ fontColour Black
                            , borderColour Black
                            ]

                        _ ->
                            [ fontColour LightGrey
                            , borderColour LightGrey
                            ]
                   )
            )
            { onPress = acceptMsg
            , label = text acceptString
            }
        , Input.button
            [ padding 5
            , Border.width 2
            , Border.solid
            , borderColour Black
            ]
            { onPress = Just cancelMsg, label = text "Cancel" }
        ]


editTextBox :
    { placeholder : String
    , label : String
    , current : String
    }
    -> String
    -> (String -> msg)
    -> Element msg
editTextBox { placeholder, label, current } type_ msg =
    Input.text
        [ width fill
        , Border.width 2
        , Border.rounded 0
        , borderColour Black
        , htmlAttribute <| HAtt.type_ type_
        ]
        { onChange = msg
        , text = current
        , placeholder = Just <| Input.placeholder [] (text placeholder)
        , label = Input.labelAbove [] (text label)
        }


linkStyle : List (Element.Attribute msg)
linkStyle =
    [ Font.underline, fontColour Blue ]


noSelect : List (Element.Attribute msg)
noSelect =
    List.map htmlAttribute
        [ HAtt.style "-webkit-touch-callout" "none"
        , HAtt.style "-webkit-user-select" "none"
        , HAtt.style "-khtml-user-select" "none"
        , HAtt.style "-moz-user-select" "none"
        , HAtt.style "-ms-user-select" "none"
        , HAtt.style "user-select" "none"
        ]


fillSpace : List (Attribute msg)
fillSpace =
    [ height fill, width fill ]


selectableOption : (Int -> msg) -> Maybe Int -> Int -> String -> Element msg
selectableOption msg mSelected index label =
    el
        [ padding 10
        , width fill
        , Font.alignLeft
        , case Maybe.map (\s -> s == index) mSelected of
            Just True ->
                backgroundColour Blue

            _ ->
                backgroundColour White
        , Events.onClick <| msg index
        ]
        (text label)
