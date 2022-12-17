import "./main.css";
import {Elm} from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

const app = Elm.Main.init({
    node: document.getElementById("root"),
});

let midiAccess = null;
let midiDevices = new Map();

navigator.requestMIDIAccess().then(onMIDISuccess, onMIDIFailure);

function onMIDISuccess(mAccess) {
    console.log("MIDI ready!");
    midiAccess = mAccess;
    initialiseDevices(midiAccess)



    midiAccess.onstatechange = function (e) {
        initialiseDevices(midiAccess)
        console.log(e.port.name + " changed state!");
    };
}

function initialiseDevices(midiAccess) {
    midiDevices = new Map();

    midiAccess.inputs.forEach((input => {
        midiDevices.set(input.name, {input: input, output: null});
    }));
    midiAccess.outputs.forEach((output => {
        console.log(output.name);
        if (output.name in midiDevices) {
            midiDevices[output.name].output = output;
        } else {
            midiDevices.set(output.name, {input: null, output: output});
        }
    }));

    let devices = Array.from(midiDevices.entries()).map(makeMidiStatusFromDevice);
    console.log(devices);
    app.ports.midiDevices.send(devices);
}

function makeMidiStatusFromDevice(entry) {
    let [name, device] = entry;
    let input = null;
    let output = null;
    if (device.input != null) {
        input = true
    }
    if (device.output != null) {
        output = true
    }
    let midiStatus = {
        name: name,
        input: input,
        output: output,
    }
    return midiStatus;
}

function onMIDIFailure(msg) {
    app.ports.listenForMIDIStatus.send(false);
    console.log("Failed to get MIDI access - " + msg);
}


app.ports.sendNoteOn.subscribe(function (midiMsg) {
    midiDevices.forEach(((device) => {
        if (device.output != null) {
            let {noteNumber, channel} = midiMsg;
            device.output.send([0x90 + channel, noteNumber, 0x7f]);
            console.log(device.output.name, "Note On", noteNumber);
        } else {
            console.log("Midi output not available for device: " + device.output.name);
        }
    }));
});

app.ports.sendNoteOff.subscribe(function (midiMsg) {
    midiDevices.forEach(((device, _) => {
        if (device.output != null) {
            let {noteNumber, channel} = midiMsg;
            device.output.send([0x80 + channel, noteNumber, 0x7f]);
            console.log(device.output.name, "Note Off", noteNumber);
        } else {
            console.log("Midi output not available for device: " + device.output.name);
        }
    }));
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.register();
