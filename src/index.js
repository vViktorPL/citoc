import ElmModule from './Main.elm';
const sfxFiles = [
  'success.mp3',
  'notify-up.mp3',
  'notify-down.mp3',
  'rumble.mp3',
  'step-1.mp3',
  'step-2.mp3',
  'step-3.mp3',
  'step-4.mp3',
  'step-5.mp3',
];

import { generateSignTexture } from './texture-generator';

const soundBuffers = {};
const audioContext = window.AudioContext && new AudioContext();

if (audioContext) {
  sfxFiles.forEach(
    (file) => {
      const url = `assets/sfx/${file}`;
      window.fetch(url)
        .then(response => response.arrayBuffer())
        .then(arrayBuffer => audioContext.decodeAudioData(
          arrayBuffer,
          audioBuffer => soundBuffers[file] = audioBuffer)
        );
    }
  );
} else {
  alert("Unfortunately, no audio support for your browser :-(");
}


(async () => {
  const { Elm } = await ElmModule;

  const app = Elm.Main.init({
    node: document.getElementById('game')
  });

  app.ports.generateTexturePort.subscribe(async ([fileName, text]) => {
    const url = await generateSignTexture(text);
    app.ports.generatedSignTextureSub.send([fileName, url]);
  });

  app.ports.playSound.subscribe(filename => {
    if (!audioContext || !(filename in soundBuffers)) {
      return;
    }

    const source = audioContext.createBufferSource();
    source.buffer = soundBuffers[filename];
    source.connect(audioContext.destination);
    source.start(0);
  });

  // Prevent scrolling with arrow and space keys
  window.addEventListener('keydown', e => {
    if ([32, 37, 38, 39, 40].indexOf(e.keyCode) > -1) {
      e.preventDefault();
    }
  }, false);

  // Capture focus
  window.focus();

  // Pointer locking
  document.addEventListener('click', function (event) {
    if (event.target.tagName === 'CANVAS') {
      const canvasElement = event.target;
      canvasElement.requestPointerLock();
    }
  });
})();
