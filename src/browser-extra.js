const SHAKE_THRESHOLD = 0.1;
const SHAKE_DURATION_REQUIRED = 500;
const PAUSE_TOLERANCE = 300;
const DIRECTION_CHANGE_TOLERANCE = 10; // Mniejsza wartość dla większej czułości na zmianę kierunku

export function subscribeToWindowShaking(callback) {
  let lastTime = Date.now();
  let lastScreenLeft = window.screenLeft;
  let lastScreenTop = window.screenTop;
  let lastDeltaX = 0;
  let lastDeltaY = 0;
  let shakeDuration = 0;
  let pauseDuration = 0;
  let directionChanges = 0;

  setInterval(() => {
    const currentTime = Date.now();
    const deltaTime = currentTime - lastTime;
    const deltaX = window.screenLeft - lastScreenLeft;
    const deltaY = window.screenTop - lastScreenTop;
    const speed = Math.sqrt(deltaX * deltaX + deltaY * deltaY) / deltaTime;

    // Sprawdzenie zmiany kierunku
    if ((deltaX * lastDeltaX < 0 && Math.abs(deltaX) > DIRECTION_CHANGE_TOLERANCE) ||
      (deltaY * lastDeltaY < 0 && Math.abs(deltaY) > DIRECTION_CHANGE_TOLERANCE)) {
      directionChanges++;
    }

    if (speed > SHAKE_THRESHOLD) {
      shakeDuration += deltaTime;
      pauseDuration = 0;
      if (shakeDuration >= SHAKE_DURATION_REQUIRED && directionChanges > 1) {
        console.log("SHAKE!");
        callback();
        shakeDuration = 0;
        directionChanges = 0;
      }
    } else {
      if (pauseDuration < PAUSE_TOLERANCE) {
        pauseDuration += deltaTime;
      } else {
        shakeDuration = 0;
        directionChanges = 0;
      }
    }

    lastTime = currentTime;
    lastDeltaX = deltaX;
    lastDeltaY = deltaY;
    lastScreenLeft = window.screenLeft;
    lastScreenTop = window.screenTop;
  }, 100);
}
