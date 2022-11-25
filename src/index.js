import "./main.css";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

const app = Elm.Main.init({
  node: document.getElementById("root"),
});
let midiOut = null;

app.ports.sendNoteOn.subscribe(function(noteNumber) {
    midiOut.send([0x94, noteNumber, 0x7f]);
    console.log("On", noteNumber);
});

app.ports.sendNoteOff.subscribe(function(noteNumber) {
    midiOut.send([0x84, noteNumber, 0x7f]);
    console.log("Off", noteNumber);
});

navigator.requestMIDIAccess().then(onMIDISuccess, onMIDIFailure);

function onMIDISuccess(midiAccess) {
  console.log("MIDI ready!");
  app.ports.listenForMIDIStatus.send(true);

  let outputs = Array.from(midiAccess.outputs.values());
  console.log(outputs);
  
  // this should be changed to outputs.get("name")
  midiOut = outputs[2];
  console.log(midiOut);

  midiAccess.onstatechange = function (e) {
    console.log(e.port.name);
  };

  // midiAccess.inputs.forEach((input) => {
  //   console.log(input.name); /* inherited property from MIDIPort */
  //   input.onmidimessage = function (message) {
  //     console.log(message.data);
  //   };
  // });
}

function onMIDIFailure(msg) {
  app.ports.listenForMIDIStatus.send(false);
  console.log("Failed to get MIDI access - " + msg);
}

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.register();
