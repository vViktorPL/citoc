import ElmModule from './Main.elm';

(async () => {
  const { Elm } = await ElmModule;

  const app = Elm.Main.init({
    node: document.getElementById('game')
  });

  // Pointer locking
  const canvas = await waitForElementToAppear('canvas');
  canvas.addEventListener('click', () => {
    canvas.requestPointerLock();
  });

})();

async function waitForElementToAppear(targetSelector) {
  return new Promise(
    resolve => {
      const observer = new MutationObserver((mutationsList, observer) => {
        const element = document.querySelector(targetSelector);
        if (element) {
          observer.disconnect();
          resolve(element);
        }
      });

      observer.observe(document.documentElement, { childList: true, subtree: true });
    }
  );
}