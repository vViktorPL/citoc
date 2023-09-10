import ElmModule from './Main.elm';

(async () => {
  const { Elm } = await ElmModule;

  const app = Elm.Main.init({
    node: document.getElementById('game')
  });
})();

