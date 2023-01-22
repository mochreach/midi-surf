import "./main.css";
import {Elm} from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

const appStorageKey = "appState";
const storedState = localStorage.getItem(appStorageKey);
const mInitialState = storedState ? JSON.parse(storedState) : null;

const app = Elm.Main.init({
    node: document.getElementById("root"),
    flags: {mInitialState: mInitialState},
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

        input.onmidimessage = (message) => {
            const deviceName = input.name;
            // Ignore clock messages
            if (message.data[0] != 0xF8) {
                app.ports.incomingMidi.send(
                    {
                        deviceName: deviceName,
                        midiData: Array.from(message.data)
                    }
                );
            }
        }
    }));

    midiAccess.outputs.forEach((output => {
        if (midiDevices.has(output.name)) {
            midiDevices.get(output.name).output = output;
        } else {
            midiDevices.set(output.name, {input: null, output: output});
        }
    }));

    let devices = Array.from(midiDevices.entries()).map(makeMidiStatusFromDevice);
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

app.ports.outgoingMidi.subscribe(function (midiMsgArray) {
    midiDevices.forEach(((device) => {
        if (device.output != null) {
            midiMsgArray.forEach(msg => {
                device.output.send(msg);
            });
        }
    }));
});

// Taken from MDN
function storageAvailable(type) {
    var storage;
    try {
        storage = window[type];
        var x = "__storage_test__";
        storage.setItem(x, x);
        storage.removeItem(x);
        return true;
    } catch (e) {
        return (
            e instanceof DOMException &&
            // everything except Firefox
            (e.code === 22 ||
                // Firefox
                e.code === 1014 ||
                // test name field too, because code might not be present
                // everything except Firefox
                e.name === "QuotaExceededError" ||
                // Firefox
                e.name === "NS_ERROR_DOM_QUOTA_REACHED") &&
            // acknowledge QuotaExceededError only if there's something already stored
            storage &&
            storage.length !== 0
        );
    }
}

app.ports.saveState.subscribe(function (state) {
    if (storageAvailable("localStorage")) {
        localStorage.setItem(appStorageKey, JSON.stringify(state));
    } else {
        console.log(
            "Storage is not available. Local storage must be enabled to save the application."
        );
    }
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.register();
