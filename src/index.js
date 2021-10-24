import "./main.css";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

Elm.Main.init({
  node: document.getElementById("root"),
});

navigator.requestMIDIAccess().then(onMIDISuccess, onMIDIFailure);

function onMIDISuccess(midiAccess) {
  console.log("MIDI ready!");
  var inputs = midiAccess.inputs.values();

  midiAccess.onstatechange = function (e) {
    console.log(e.port.name, e.port.manufacturer, e.port.state);
  };

  // midiAccess.inputs.forEach((input) => {
  //   console.log(input.name); /* inherited property from MIDIPort */
  //   input.onmidimessage = function (message) {
  //     console.log(message.data);
  //   };
  // });
}

function onMIDIFailure(msg) {
  console.log("Failed to get MIDI access - " + msg);
}

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.register();
