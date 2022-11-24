module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Ports exposing (listenForMIDIStatus)



---- MODEL ----


type alias Model =
    { midiStatus : MIDIStatus
    , page : Page
    }


type MIDIStatus
    = Initialising
    | FailedToEstablishMIDI
    | MIDIConnected


type alias Page =
    { label : String
    , gapSize : Int
    , controller : Controller
    }


type Controller
    = Module String Controller
    | Row (List Controller)
    | Column (List Controller)
    | Control MidiControl


type MidiControl
    = Button



-- | Slider


init : ( Model, Cmd Msg )
init =
    ( { midiStatus = Initialising
      , page = defaultPage
      }
    , Cmd.none
    )


defaultPage : Page
defaultPage =
    { label = "1"
    , gapSize = 5
    , controller =
        Control Button
            |> List.repeat 12
            |> Row
            |> List.repeat 3
            |> Column
    }



---- UPDATE ----


type Msg
    = MIDIStatusChanged Bool
    | ButtonDown
    | ButtonUp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MIDIStatusChanged isConnected ->
            ( if isConnected then
                { model | midiStatus = MIDIConnected }

              else
                { model | midiStatus = FailedToEstablishMIDI }
            , Cmd.none
            )

        ButtonDown ->
            let
                _ =
                    Debug.log "Pressed a button..." "I did too"
            in
            ( model, Cmd.none )

        ButtonUp ->
            let
                _ =
                    Debug.log "Released a button..." "I did too"
            in
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Element Msg
view model =
    column ([ padding 5 ] ++ fillSpace)
        [ midiStatus model.midiStatus
        , renderPage model.page
        ]


midiStatus : MIDIStatus -> Element Msg
midiStatus status =
    case status of
        Initialising ->
            el [] <| text "Initialising..."

        FailedToEstablishMIDI ->
            el [] <| text "Failed to establish MIDI connection."

        MIDIConnected ->
            el [] <| text "MIDI connection sucessful!"


renderPage : Page -> Element Msg
renderPage { label, gapSize, controller } =
    el ([ padding gapSize, Border.solid, Border.width 2 ] ++ fillSpace) <|
        renderController gapSize controller


renderController : Int -> Controller -> Element Msg
renderController gapSize controller =
    case controller of
        Module _ _ ->
            el
                ([ padding gapSize
                 , spacing gapSize
                 , Background.color <| rgb255 3 5 5
                 ]
                    ++ fillSpace
                )
                none

        Row subControls ->
            row
                ([ padding gapSize
                 , spacing gapSize
                 , Background.color <| rgb255 21 39 200
                 ]
                    ++ fillSpace
                )
            <|
                List.map (renderController gapSize) subControls

        Column subControls ->
            column
                ([ padding gapSize
                 , spacing gapSize
                 , Background.color <| rgb255 21 221 23
                 ]
                    ++ fillSpace
                )
            <|
                List.map (renderController gapSize) subControls

        Control midiControl ->
            renderMidiControl gapSize midiControl


renderMidiControl : Int -> MidiControl -> Element Msg
renderMidiControl gapSize midiControl =
    case midiControl of
        Button ->
            el
                ([ padding gapSize
                 , spacing gapSize
                 , Background.color <| rgb255 221 221 23
                 , htmlAttribute <| Touch.onStart (\_ -> ButtonDown)
                 , htmlAttribute <| Mouse.onDown (\_ -> ButtonDown)
                 , htmlAttribute <| Touch.onEnd (\_ -> ButtonUp)
                 , htmlAttribute <| Mouse.onUp (\_ -> ButtonUp)
                 ]
                    ++ fillSpace
                )
                none


fillSpace : List (Attribute msg)
fillSpace =
    [ height fill, width fill ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view >> layout []
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    listenForMIDIStatus MIDIStatusChanged
