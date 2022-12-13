module Main exposing (..)

import Browser
import Controller exposing (Controller, updateWithId)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import FeatherIcons as Icons
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Ports



-- {{{ MODEL


type alias Model =
    { midiStatus : MIDIStatus
    , mode : Mode
    , page : Page
    , popup : Maybe PopUp
    }


type MIDIStatus
    = Initialising
    | FailedToEstablishMIDI
    | MidiAvailable (List ( String, String ))
    | MidiConnected String


type Mode
    = Normal
    | Edit


type PopUp
    = MidiMenu MidiMenuModel
    | EditMenu String EditMenuState


type alias MidiMenuModel =
    { devices : List ( String, String )
    , selected : Maybe String
    }


type EditMenuState
    = EditButton EditButtonState


type alias Page =
    { label : String
    , controller : Controller
    , config : PageConfig
    }


type alias PageConfig =
    { gapSize : Int
    , debug : Bool
    }


type alias EditButtonState =
    { label : String
    , noteNumber : String
    }


editStateToButton : EditButtonState -> Maybe Controller
editStateToButton { label, noteNumber } =
    case ( String.isEmpty label, String.toInt noteNumber ) of
        ( False, Just nn ) ->
            Controller.newButton label nn
                |> Just

        _ ->
            Nothing


validLabel : EditButtonState -> Bool
validLabel { label } =
    not <| String.isEmpty label


init : ( Model, Cmd Msg )
init =
    ( { midiStatus = Initialising
      , mode = Normal
      , page = defaultPage
      , popup = Nothing
      }
    , Cmd.none
    )


defaultPage : Page
defaultPage =
    { label = "1"
    , controller = isomorphicKeyboard
    , config =
        { gapSize = 2
        , debug = True
        }
    }


isomorphicKeyboard : Controller
isomorphicKeyboard =
    let
        noteRange =
            List.range 36 127

        rowNumbers =
            List.range 0 10
    in
    List.map (makeIsomorphicRow noteRange 5 18) rowNumbers
        |> List.reverse
        |> Controller.Column


makeIsomorphicRow : List Int -> Int -> Int -> Int -> Controller
makeIsomorphicRow noteRange offset rowLength rowNumber =
    let
        start =
            offset * rowNumber

        includedRange =
            List.range start (start + rowLength)
    in
    noteRange
        |> List.indexedMap Tuple.pair
        |> List.filter (\( i, _ ) -> List.member i includedRange)
        |> List.map Tuple.second
        |> List.map (\i -> Controller.newButton (String.fromInt i) i)
        |> Controller.Row



-- }}}
-- {{{ UPDATE


type Msg
    = MIDIStatusChanged (List ( String, String ))
    | ToggleNormalEdit
    | OpenMidiMenu
    | ConnectToDevice String
    | ConnectedToDevice String
    | AddSpace String
    | RemoveItem String
    | OpenEditController String
    | FinishedEdit Controller
    | UpdateControllerState EditMenuState
    | ButtonDown String
    | ButtonUp String
    | ClosePopUp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MIDIStatusChanged devices ->
            ( if List.isEmpty devices |> not then
                { model | midiStatus = MidiAvailable devices }

              else
                { model | midiStatus = FailedToEstablishMIDI }
            , Cmd.none
            )

        ToggleNormalEdit ->
            ( { model
                | mode =
                    case model.mode of
                        Edit ->
                            Normal

                        Normal ->
                            Edit
              }
            , Cmd.none
            )

        OpenMidiMenu ->
            ( { model
                | popup =
                    Just <|
                        MidiMenu
                            { devices =
                                case model.midiStatus of
                                    MidiAvailable devices ->
                                        devices

                                    _ ->
                                        []
                            , selected = Nothing
                            }
              }
            , Cmd.none
            )

        ConnectToDevice id ->
            ( model
            , Ports.connectToDevice id
            )

        ConnectedToDevice name ->
            ( { model
                | midiStatus = MidiConnected name
                , popup = Nothing
              }
            , Cmd.none
            )

        AddSpace id ->
            let
                page =
                    model.page

                updatedPage =
                    { page
                        | controller =
                            Controller.updateWithId
                                "0"
                                page.controller
                                { id = id, updateFn = Controller.addSpace }
                    }
            in
            ( { model | page = updatedPage }
            , Cmd.none
            )

        RemoveItem id ->
            let
                page =
                    model.page

                updatedPage =
                    { page
                        | controller =
                            Controller.updateWithId
                                "0"
                                page.controller
                                { id = id, updateFn = Controller.removeItem }
                    }
            in
            ( { model | page = updatedPage }
            , Cmd.none
            )

        OpenEditController id ->
            let
                page =
                    model.page

                controller =
                    Controller.getWithId "0" id page.controller
            in
            ( { model
                | popup =
                    case controller of
                        Just (Controller.Button { noteNumber, label }) ->
                            Just <|
                                EditMenu id <|
                                    EditButton
                                        { noteNumber = String.fromInt noteNumber
                                        , label = label
                                        }

                        _ ->
                            Nothing
              }
            , Cmd.none
            )

        FinishedEdit controller ->
            case model.popup of
                Just (EditMenu id _) ->
                    let
                        page =
                            model.page

                        updatedPage =
                            { page
                                | controller =
                                    Controller.updateWithId
                                        "0"
                                        page.controller
                                        { id = id, updateFn = \_ -> controller }
                            }
                    in
                    ( { model
                        | popup = Nothing
                        , page = updatedPage
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | popup = Nothing }
                    , Cmd.none
                    )

        UpdateControllerState state ->
            case model.popup of
                Just (EditMenu id _) ->
                    ( { model
                        | popup =
                            Just <|
                                EditMenu id state
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | popup = Nothing }
                    , Cmd.none
                    )

        ButtonDown id ->
            let
                page =
                    model.page

                button =
                    Controller.getWithId "0" id page.controller

                updatedPage =
                    { page
                        | controller =
                            Controller.updateWithId
                                "0"
                                page.controller
                                { id = id, updateFn = Controller.buttonOn }
                    }
            in
            ( { model | page = updatedPage }
            , case button of
                Just (Controller.Button state) ->
                    Ports.sendNoteOn state.noteNumber

                _ ->
                    Cmd.none
            )

        ButtonUp id ->
            let
                page =
                    model.page

                button =
                    Controller.getWithId "0" id page.controller

                updatedPage =
                    { page
                        | controller =
                            Controller.updateWithId
                                "0"
                                page.controller
                                { id = id, updateFn = Controller.buttonOff }
                    }
            in
            ( { model | page = updatedPage }
            , case button of
                Just (Controller.Button state) ->
                    Ports.sendNoteOff state.noteNumber

                _ ->
                    Cmd.none
            )

        ClosePopUp ->
            ( { model | popup = Nothing }
            , Cmd.none
            )



-- }}}
-- {{{ VIEW


view : Model -> Html Msg
view model =
    layout
        ((case model.popup of
            Just popup ->
                (inFront <|
                    case popup of
                        MidiMenu state ->
                            el
                                (Background.color (rgba 0.5 0.5 0.5 0.8)
                                    :: fillSpace
                                )
                                (midiMenu state.devices)

                        EditMenu _ state ->
                            el
                                (Background.color (rgba 0.5 0.5 0.5 0.8)
                                    :: fillSpace
                                )
                                (editMenu state)
                )
                    :: fillSpace

            Nothing ->
                fillSpace
         )
            ++ [ Font.family
                    [ Font.external
                        { name = "Space Mono"
                        , url = "https://fonts.googleapis.com/css?family=Space+Mono"
                        }
                    , Font.monospace
                    ]
               ]
        )
    <|
        row
            fillSpace
            [ column
                [ height fill
                , padding 5
                , spacing 5
                , Border.widthEach { bottom = 0, top = 0, left = 0, right = 4 }
                ]
                [ Input.button
                    [ padding 10
                    , Border.width 4
                    ]
                    { onPress = Just OpenMidiMenu
                    , label =
                        Icons.gitPullRequest
                            |> Icons.withSize 36
                            |> Icons.toHtml []
                            |> html
                    }
                , Input.button
                    [ padding 10
                    , Border.width 4
                    , Background.color <|
                        case model.mode of
                            Normal ->
                                rgb 1.0 1.0 1.0

                            Edit ->
                                rgb 0.4 0.4 0.4
                    ]
                    { onPress = Just ToggleNormalEdit
                    , label =
                        Icons.edit
                            |> Icons.withSize 36
                            |> Icons.toHtml []
                            |> html
                    }
                ]
            , column
                (padding 5 :: fillSpace)
                [ midiStatus model.midiStatus
                , renderPage model.mode model.page
                ]
            ]


midiStatus : MIDIStatus -> Element Msg
midiStatus status =
    case status of
        Initialising ->
            paragraph [] [ text "Initialising..." ]

        FailedToEstablishMIDI ->
            paragraph [] [ text "Failed to establish MIDI connection." ]

        MidiAvailable devices ->
            paragraph [] [ text <| "MIDI connection: " ++ String.fromInt (List.length devices) ]

        MidiConnected device ->
            paragraph [] [ text <| "MIDI Out: " ++ device ]


midiMenu : List ( String, String ) -> Element Msg
midiMenu devices =
    el [ centerX, centerY ] <|
        column
            [ padding 10
            , spacing 10
            , Background.color (rgb 1.0 1.0 1.0)
            ]
            (paragraph [] [ text "Select MIDI Device" ]
                :: (case devices of
                        [] ->
                            [ paragraph [] [ text "No MIDI devices connected." ] ]

                        _ ->
                            List.map deviceItem devices
                   )
                ++ [ Input.button
                        [ padding 5
                        , Border.width 2
                        , Border.solid
                        , Border.color <| rgb255 0 0 0
                        ]
                        { onPress = Just ClosePopUp, label = text "Cancel" }
                   ]
            )


editMenu : EditMenuState -> Element Msg
editMenu menuType =
    case menuType of
        EditButton state ->
            el [ centerX, centerY ] <|
                column
                    [ padding 10
                    , spacing 10
                    , Background.color (rgb 1.0 1.0 1.0)
                    ]
                    [ Input.text
                        [ Border.width 2
                        , Border.rounded 0
                        , Border.color (rgb 0.0 0.0 0.0)
                        ]
                        { onChange =
                            \newLabel ->
                                { state | label = newLabel }
                                    |> EditButton
                                    |> UpdateControllerState
                        , text = state.label
                        , placeholder = Just <| Input.placeholder [] (text "enter label")
                        , label = Input.labelAbove [] (text "Label")
                        }
                    , Input.text
                        [ Border.width 2
                        , Border.rounded 0
                        , Border.color (rgb 0.0 0.0 0.0)
                        ]
                        { onChange =
                            \newNoteNumber ->
                                { state | noteNumber = newNoteNumber }
                                    |> EditButton
                                    |> UpdateControllerState
                        , text = state.noteNumber
                        , placeholder = Just <| Input.placeholder [] (text "enter note#")
                        , label = Input.labelAbove [] (text "Note Number")
                        }
                    , case editStateToButton state of
                        Just controller ->
                            Input.button
                                [ padding 5
                                , Border.width 2
                                , Border.solid
                                , Border.color <| rgb 0.7 0.7 0.7
                                ]
                                { onPress = Just <| FinishedEdit controller
                                , label = text "Ok"
                                }

                        Nothing ->
                            Input.button
                                [ padding 5
                                , Border.width 2
                                , Border.solid
                                , Border.color <| rgb 0 0 0
                                ]
                                { onPress = Nothing
                                , label = text "Ok"
                                }
                    , Input.button
                        [ padding 5
                        , Border.width 2
                        , Border.solid
                        , Border.color <| rgb255 0 0 0
                        ]
                        { onPress = Just ClosePopUp, label = text "Cancel" }
                    ]


deviceItem : ( String, String ) -> Element Msg
deviceItem ( id, name ) =
    Input.button
        [ padding 10
        , width fill
        , Background.color <| rgb 0.8 0.8 0.8
        ]
        { onPress = Just <| ConnectToDevice id
        , label = text name
        }


renderPage : Mode -> Page -> Element Msg
renderPage mode page =
    let
        { config, controller } =
            page
    in
    el
        (fillSpace
            ++ (case mode of
                    Normal ->
                        []

                    Edit ->
                        [ paddingXY config.gapSize 0
                        ]
               )
        )
    <|
        renderController mode config [] controller 0


renderController : Mode -> PageConfig -> List String -> Controller -> Int -> Element Msg
renderController mode config idParts controller id =
    let
        updatedParts =
            String.fromInt id :: idParts
    in
    case controller of
        Controller.Module _ subControls ->
            Lazy.lazy2
                el
                ([ padding config.gapSize
                 , spacing config.gapSize
                 , Border.dotted
                 , Border.width 2
                 ]
                    ++ fillSpace
                )
                (renderController mode config updatedParts subControls 0)

        Controller.Row subControls ->
            case mode of
                Normal ->
                    Lazy.lazy2 row
                        ([ paddingXY config.gapSize 0
                         , spacingXY config.gapSize 0
                         ]
                            ++ fillSpace
                        )
                    <|
                        List.map2
                            (renderController mode config updatedParts)
                            subControls
                            (List.range 0 <| List.length subControls)

                Edit ->
                    Lazy.lazy2 row
                        ([ paddingXY 5 0
                         , spacing 5
                         , Border.width 2
                         , Border.dashed
                         ]
                            ++ fillSpace
                        )
                        [ renderEditButton config
                            Controller.Remove
                            (updatedParts
                                |> List.reverse
                                |> String.join "_"
                            )
                        , row
                            ([ spacingXY config.gapSize 0
                             , padding config.gapSize
                             ]
                                ++ fillSpace
                            )
                          <|
                            List.map2
                                (renderController mode config updatedParts)
                                subControls
                                (List.range 0 <| List.length subControls)
                        , renderEditButton
                            config
                            Controller.Add
                            (updatedParts
                                |> List.reverse
                                |> String.join "_"
                            )
                        ]

        Controller.Column subControls ->
            case mode of
                Normal ->
                    Lazy.lazy2 column
                        ([ paddingXY 0 config.gapSize
                         , spacingXY 0 config.gapSize
                         ]
                            ++ fillSpace
                        )
                    <|
                        List.map2
                            (renderController mode config updatedParts)
                            subControls
                            (List.range 0 <| List.length subControls)

                Edit ->
                    Lazy.lazy2 column
                        ([ paddingXY 0 5
                         , spacing 5
                         , Border.width 2
                         , Border.dashed
                         ]
                            ++ fillSpace
                        )
                        [ renderEditButton config
                            Controller.Remove
                            (updatedParts
                                |> List.reverse
                                |> String.join "_"
                            )
                        , column
                            ([ spacingXY 0 config.gapSize
                             , padding config.gapSize
                             ]
                                ++ fillSpace
                            )
                          <|
                            List.map2
                                (renderController mode config updatedParts)
                                subControls
                                (List.range 0 <| List.length subControls)
                        , renderEditButton
                            config
                            Controller.Add
                            (updatedParts
                                |> List.reverse
                                |> String.join "_"
                            )
                        ]

        Controller.Button state ->
            renderButton
                config
                mode
                state
                (updatedParts
                    |> List.reverse
                    |> String.join "_"
                )

        Controller.Space ->
            el ((Background.color <| rgb 0.9 0.9 0.9) :: fillSpace) none


renderButton : PageConfig -> Mode -> Controller.ButtonState -> String -> Element Msg
renderButton config mode state id =
    case mode of
        Normal ->
            el
                ([ padding 0
                 , spacing 0
                 , Border.width 2

                 -- , Border.solid
                 , case state.status of
                    Controller.Off ->
                        if modBy 12 state.noteNumber == 0 then
                            Background.color <| rgb255 100 100 200

                        else if List.member (modBy 12 state.noteNumber) [ 1, 3, 6, 8, 10 ] then
                            Background.color <| rgb255 170 170 18

                        else
                            Background.color <| rgb255 221 221 23

                    Controller.On ->
                        Background.color <| rgb255 (221 // 2) (221 // 2) (23 // 2)
                 , htmlAttribute <|
                    Touch.onStart
                        (\_ ->
                            ButtonDown id
                        )
                 , htmlAttribute <|
                    Mouse.onDown
                        (\_ ->
                            ButtonDown id
                        )
                 , htmlAttribute <|
                    Touch.onEnd
                        (\_ ->
                            ButtonUp id
                        )
                 , htmlAttribute <|
                    Mouse.onUp
                        (\_ ->
                            ButtonUp id
                        )
                 ]
                    ++ fillSpace
                )
                ((if config.debug then
                    case state.status of
                        Controller.Off ->
                            "Off\n"

                        Controller.On ->
                            "On\n"

                  else
                    ""
                 )
                    ++ state.label
                    |> text
                    |> el
                        [ centerX
                        , centerY
                        , Font.size 14
                        ]
                )

        Edit ->
            Input.button
                ([ padding config.gapSize
                 , spacing config.gapSize
                 , Border.width 2
                 , Border.dashed
                 , Font.size 14
                 , if modBy 12 state.noteNumber == 0 then
                    Background.color <| rgb255 100 100 200

                   else if List.member (modBy 12 state.noteNumber) [ 1, 3, 6, 8, 10 ] then
                    Background.color <| rgb255 170 170 18

                   else
                    Background.color <| rgb255 221 221 23
                 ]
                    ++ fillSpace
                )
                { onPress = Just <| OpenEditController id
                , label =
                    state.label
                        |> text
                }


renderEditButton : PageConfig -> Controller.EditOperation -> String -> Element Msg
renderEditButton config editOperation parentId =
    case editOperation of
        Controller.Add ->
            Input.button
                [ centerX
                , padding config.gapSize
                , spacing config.gapSize
                , Border.width 2
                ]
                { onPress = Just <| AddSpace parentId
                , label =
                    Icons.plus
                        |> Icons.withSize 36
                        |> Icons.toHtml []
                        |> html
                }

        Controller.Remove ->
            Input.button
                [ centerX
                , padding config.gapSize
                , spacing config.gapSize
                , Border.width 2
                ]
                { onPress = Just <| RemoveItem parentId
                , label =
                    Icons.minus
                        |> Icons.withSize 36
                        |> Icons.toHtml []
                        |> html
                }


fillSpace : List (Attribute msg)
fillSpace =
    [ height fill, width fill ]



-- }}}
-- {{{ PROGRAM


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.listenForMIDIStatus MIDIStatusChanged
        , Ports.connectedToDevice ConnectedToDevice
        ]



-- }}}
