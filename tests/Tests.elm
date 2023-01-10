module Tests exposing (..)

import Codec
import Expect
import Main exposing (modelCodec)
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "Test Suite for MIDI Surf"
        [ test "Load version 0.1.0 Model from JSON string" <|
            \_ ->
                Codec.decodeString modelCodec everything_0_1_0
                    |> Expect.ok
        ]


everything_0_1_0 : String
everything_0_1_0 =
    """{"midiLog":null,"popup":null,"menuOpen":null,"savedModules":{},"savedPages":{},"activePage":null,"pages":[{"config":{"debug":false,"gapSize":2},"controller":{"tag":"Module","args":["Everything",{"tag":"Column","args":[[{"tag":"Row","args":[[{"tag":"MidiLog","args":[]},{"tag":"Space","args":[]}]]},{"tag":"Row","args":[[{"tag":"Note","args":[{"velocity":100,"pitch":60,"channel":{"tag":"Ch1","args":[]},"colour":{"tag":"Green","args":[]},"label":"Note","status":null}]},{"tag":"Chord","args":[{"notes":[{"pitch":60,"channel":{"tag":"Ch8","args":[]}},{"pitch":64,"channel":{"tag":"Ch8","args":[]}},{"pitch":67,"channel":{"tag":"Ch8","args":[]}}],"velocity":100,"colour":{"tag":"Blue","args":[]},"label":"Chord","status":null}]},{"tag":"CCValue","args":[{"value":1,"controller":53,"channel":{"tag":"Ch6","args":[]},"colour":{"tag":"Yellow","args":[]},"label":"CC Msg","status":null}]},{"tag":"Fader","args":[{"valueMax":127,"valueMin":0,"valuePercent":50,"ccNumber":3,"channel":{"tag":"Ch8","args":[]},"colour":{"tag":"Red","args":[]},"label":"Fader","status":null}]}]]}]]}]},"label":"Everything 0.1.0"}],"mode":null,"midiStatus":null}"""
