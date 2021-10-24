module Main exposing (..)

import Browser
import Html exposing (Html, div, text)
import Ports exposing (listenForMIDIStatus)



---- MODEL ----


type alias Model =
    { midiStatus : MIDIStatus }


type MIDIStatus
    = Initialising
    | FailedToEstablishMIDI
    | MIDIConnected


init : ( Model, Cmd Msg )
init =
    ( { midiStatus = Initialising }, Cmd.none )



---- UPDATE ----


type Msg
    = MIDIStatusChanged Bool


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



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.midiStatus of
        Initialising ->
            div [] [ text "Initialising..." ]

        FailedToEstablishMIDI ->
            div [] [ text "Failed to establish MIDI connection." ]

        MIDIConnected ->
            div [] [ text "MIDI connection sucessful!" ]



---- PROGRAM ----


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
    listenForMIDIStatus MIDIStatusChanged
