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
        , test "Load version 0.2.0 Model from JSON string" <|
            \_ ->
                Codec.decodeString modelCodec everything_0_2_0
                    |> Expect.ok
        ]


everything_0_1_0 : String
everything_0_1_0 =
    """{"midiLog":null,"popup":null,"menuOpen":null,"savedModules":{},"savedPages":{},"activePage":null,"pages":[{"config":{"debug":false,"gapSize":2},"controller":{"tag":"Module","args":["Everything",{"tag":"Column","args":[[{"tag":"Row","args":[[{"tag":"MidiLog","args":[]},{"tag":"Space","args":[]}]]},{"tag":"Row","args":[[{"tag":"Note","args":[{"velocity":100,"pitch":60,"channel":{"tag":"Ch1","args":[]},"colour":{"tag":"Green","args":[]},"label":"Note","status":null}]},{"tag":"Chord","args":[{"notes":[{"pitch":60,"channel":{"tag":"Ch8","args":[]}},{"pitch":64,"channel":{"tag":"Ch8","args":[]}},{"pitch":67,"channel":{"tag":"Ch8","args":[]}}],"velocity":100,"colour":{"tag":"Blue","args":[]},"label":"Chord","status":null}]},{"tag":"CCValue","args":[{"value":1,"controller":53,"channel":{"tag":"Ch6","args":[]},"colour":{"tag":"Yellow","args":[]},"label":"CC Msg","status":null}]},{"tag":"Fader","args":[{"valueMax":127,"valueMin":0,"valuePercent":50,"ccNumber":3,"channel":{"tag":"Ch8","args":[]},"colour":{"tag":"Red","args":[]},"label":"Fader","status":null}]}]]}]]}]},"label":"Everything 0.1.0"}],"mode":null,"midiStatus":null}"""


everything_0_2_0 : String
everything_0_2_0 =
    """{"screen":{"height":979,"width":948},"midiLog":null,"popup":null,"menuOpen":null,"savedModules":{},"savedPages":{},"activePage":null,"pages":[{"config":{"debug":false,"gapSize":2},"controller":{"tag":"Module","args":["Everything",{"tag":"Column","args":[[{"tag":"Row","args":[[{"tag":"Note","args":[{"velocity":100,"pitch":60,"channel":{"tag":"Ch8","args":[]},"colour":{"tag":"Green","args":[]},"labelSize":{"tag":"Medium","args":[]},"label":"Note","status":null}]},{"tag":"Chord","args":[{"notes":[{"pitch":60,"channel":{"tag":"Ch7","args":[]}},{"pitch":64,"channel":{"tag":"Ch7","args":[]}},{"pitch":67,"channel":{"tag":"Ch7","args":[]}}],"velocity":100,"colour":{"tag":"Blue","args":[]},"labelSize":{"tag":"Medium","args":[]},"label":"Chord","status":null}]},{"tag":"CCValue","args":[{"value":39,"controller":1,"channel":{"tag":"Ch5","args":[]},"colour":{"tag":"Yellow","args":[]},"labelSize":{"tag":"Large","args":[]},"label":"CCValue","status":null}]},{"tag":"Command","args":[{"onReleaseMsgs":[{"tag":"ControllerChange","args":[{"value":0,"controller":1,"channel":5}]},{"tag":"StopSong","args":[]}],"onPressMsgs":[{"tag":"ControllerChange","args":[{"value":111,"controller":1,"channel":5}]},{"tag":"StartSong","args":[]}],"colour":{"tag":"White","args":[]},"labelSize":{"tag":"ExtraLarge","args":[]},"label":"Command","status":null}]},{"tag":"Sequence","args":[{"index":0,"midiMsgs":[{"tag":"NoteOn","args":[{"velocity":100,"pitch":60,"channel":5}]},{"tag":"NoteOff","args":[{"velocity":0,"pitch":60,"channel":5}]}],"colour":{"tag":"DarkGrey","args":[]},"labelSize":{"tag":"Medium","args":[]},"label":"Sequence","status":null}]}]]},{"tag":"Row","args":[[{"tag":"Fader","args":[{"valueMax":127,"valueMin":0,"valuePercent":69,"ccNumber":8,"channel":{"tag":"Ch5","args":[]},"colour":{"tag":"Yellow","args":[]},"labelSize":{"tag":"Medium","args":[]},"label":"Fader","status":null}]},{"tag":"XYFader","args":[{"valueMax2":127,"valueMin2":0,"valuePercent2":60,"ccNumber2":2,"channel2":{"tag":"Ch5","args":[]},"valueMax1":127,"valueMin1":0,"valuePercent1":72,"ccNumber1":1,"channel1":{"tag":"Ch5","args":[]},"colour":{"tag":"Blue","args":[]},"labelSize":{"tag":"Medium","args":[]},"label":"XYFader","status":null}]},{"tag":"PitchBend","args":[{"bendValue":null,"channel":{"tag":"Ch5","args":[]},"colour":{"tag":"Red","args":[]},"labelSize":{"tag":"Medium","args":[]},"label":"Pitch Bend","status":null}]},{"tag":"MidiLog","args":[]},{"tag":"Space","args":[]}]]}]]}]},"label":"Everything 0.2.0"}],"mode":null,"midiStatus":null}"""
