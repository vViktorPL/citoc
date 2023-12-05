import ElmModule from './Main.elm';

import { generateSignTexture } from './texture-generator';
import { subscribeToWindowShaking, controlClipboard } from './browser-extra';

const soundBuffers = {};
const musicBuffers = {};
const audioContext = window.AudioContext && new AudioContext();
let isAudioContextStarted = false;

function startAudioContext() {
  if (!isAudioContextStarted && audioContext.state === 'suspended') {
    audioContext.resume().then(() => {
      isAudioContextStarted = true;
    });
  }
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

  // Sound port
  app.ports.preloadSound.subscribe(async filename => {
    if (audioContext) {
      const url = filename.startsWith('narration') ? `assets/narration/${filename}` : `assets/sfx/${filename}`;
      await window.fetch(url)
        .then(response => response.arrayBuffer())
        .then(arrayBuffer => audioContext.decodeAudioData(
          arrayBuffer,
          audioBuffer => soundBuffers[filename] = audioBuffer)
        ).catch(e => {
          console.error(`Couldn't load sfx: ${filename}`, e);
        });
    }
    app.ports.soundPreloadedSub.send(filename);
  })

  app.ports.playSound.subscribe(filename => {
    if (!audioContext || !(filename in soundBuffers)) {
      return;
    }

    const source = audioContext.createBufferSource();
    source.buffer = soundBuffers[filename];
    source.connect(audioContext.destination);
    source.start(0);
  });

  // Music port
  app.ports.preloadMusic.subscribe(async filename => {
    if (audioContext) {
      const url = `assets/music/${filename}`;
      await window.fetch(url)
        .then(response => response.arrayBuffer())
        .then(arrayBuffer => audioContext.decodeAudioData(
          arrayBuffer,
          musicBuffer => musicBuffers[filename] = musicBuffer)
        ).catch(e => {
          console.error(`Couldn't load music: ${filename}`, e);
        });
    }
    app.ports.musicPreloadedSub.send(filename);
  });

  app.ports.playMusic.subscribe(filename => {
    if (!audioContext || !(filename in musicBuffers)) {
      return;
    }

    playMusic(filename);
  });
  app.ports.stopMusic.subscribe(() => {
    if (!audioContext) {
      return;
    }

    fadeoutMusic();
  })

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

  document.addEventListener('click', () => {
    startAudioContext();
  });

  subscribeToWindowShaking(() => app.ports.windowShakeInternal.send(null))

  const clipboardController = controlClipboard({
    onCopy() {
      app.ports.clipboardEventInternal.send(['copy', null]);
    },
    onCut() {
      app.ports.clipboardEventInternal.send(['cut', null]);
    },
    onPaste(text) {
      app.ports.clipboardEventInternal.send(['paste', text]);
    }
  });

  app.ports.setClipboardCopyableText.subscribe(
    text => clipboardController.setCurrentCopyableText(text)
  );
})();

// MUSIC
let currentMusicSource = null;
let currentGainNode = null;
let musicLoop = false;

async function playMusic(filename) {
  if (filename.startsWith('ending')) {
    await stopMusic();
  } else {
    await fadeoutMusic();
  }
  startNewMusic(filename);
}

async function stopMusic() {
  if (currentMusicSource && currentGainNode) {
    musicLoop = false;
    currentMusicSource.onended = null;
    currentMusicSource.stop();
  }
}

async function fadeoutMusic() {
  if (currentMusicSource && currentGainNode) {

    const fadeOutDuration = 1.5;
    const currentTime = audioContext.currentTime;

    musicLoop = false;
    currentMusicSource.onended = null;
    currentGainNode.gain.setValueAtTime(currentGainNode.gain.value, currentTime);
    currentGainNode.gain.linearRampToValueAtTime(0, currentTime + fadeOutDuration);
    currentMusicSource.stop(currentTime + fadeOutDuration);

    await new Promise(resolve => setTimeout(() => resolve(), fadeOutDuration * 1000));
  }
}

function startNewMusic(filename, fadeInDuration = 0) {
  musicLoop = true;
  startAudioContext();

  const gainNode = audioContext.createGain();
  currentMusicSource = audioContext.createBufferSource();
  currentMusicSource.buffer = musicBuffers[filename];
  currentMusicSource.connect(gainNode).connect(audioContext.destination);
  gainNode.gain.setValueAtTime(0.5, audioContext.currentTime);
  currentMusicSource.start(0);

  currentGainNode = gainNode;

  currentMusicSource.onended = function() {
    currentMusicSource = null;
    currentGainNode = null;

    if (musicLoop) {
      startNewMusic(filename);
    }
  };
}
