import "./main.css";
import {Elm} from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

const app = Elm.Main.init({
    node: document.getElementById("root"),
});

let midiAccess = null;
let midiOut = null;

app.ports.connectToDevice.subscribe(function (id) {
    if (midiAccess) {
        midiOut = midiAccess.outputs.get(id);
        if (midiOut) {
            console.log(midiOut);
            app.ports.connectedToDevice.send(midiOut.name);
        }
    }
});

app.ports.sendNoteOn.subscribe(function (midiMsg) {
    if (midiOut) {
        let {noteNumber, channel} = midiMsg;
        console.log(noteNumber, channel);
        midiOut.send([0x90 + channel, noteNumber, 0x7f]);
        console.log("On", noteNumber);
    } else {
        console.log("midiOut undefined: Cannot send note off.");
    }
});

app.ports.sendNoteOff.subscribe(function (midiMsg) {
    if (midiOut) {
        let {noteNumber, channel} = midiMsg;
        midiOut.send([0x80 + channel, noteNumber, 0x7f]);
        console.log("Off", noteNumber);
    } else {
        console.log("midiOut undefined: Cannot send note off.");
    }
});

navigator.requestMIDIAccess().then(onMIDISuccess, onMIDIFailure);

function onMIDISuccess(mAccess) {
    console.log("MIDI ready!");
    midiAccess = mAccess;
    let midiDevices = Array.from(midiAccess.outputs.values());
    app.ports.listenForMIDIStatus.send(midiDevices.map(x => [x.id, x.name]));

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
