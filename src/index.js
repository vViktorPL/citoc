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
  'sand-step.mp3',
  'elevator_door.mp3',
  'glass-break.mp3',
  'narration_1.mp3',
  'narration_2.mp3',
  'narration_3.mp3',
  'narration_4.mp3',
  'narration_5.mp3',
  'narration_6.mp3',
  'narration_7.mp3',
  'narration_8.mp3',
  'narration_9.mp3',
  'narration_10.mp3',
  'narration_11.mp3',
  'narration_12.mp3',
  'narration_13.mp3',
  'narration_14.mp3'
];
const musicFiles = [
  'menu.mp3',
  'first-level.mp3',
  'ending_loop0.mp3',
  'ending_loop1.mp3',
  'ending_loop2.mp3',
  'ending_loop3.mp3',
  'ending_loop4.mp3',
  'thx-song.mp3'
];

import { generateSignTexture } from './texture-generator';

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

  if (audioContext) {
    for (const file of sfxFiles) {
      const url = file.startsWith('narration') ? `assets/narration/${file}` : `assets/sfx/${file}`;
      await window.fetch(url)
        .then(response => response.arrayBuffer())
        .then(arrayBuffer => audioContext.decodeAudioData(
          arrayBuffer,
          audioBuffer => soundBuffers[file] = audioBuffer)
        );
    }

    for (const file of musicFiles) {
      const url = `assets/music/${file}`;
      await window.fetch(url)
        .then(response => response.arrayBuffer())
        .then(arrayBuffer => audioContext.decodeAudioData(
          arrayBuffer,
          musicBuffer => musicBuffers[file] = musicBuffer)
        );
    }
  }

  const app = Elm.Main.init({
    node: document.getElementById('game')
  });

  app.ports.generateTexturePort.subscribe(async ([fileName, text]) => {
    const url = await generateSignTexture(text);
    app.ports.generatedSignTextureSub.send([fileName, url]);
  });

  // Sound port
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
