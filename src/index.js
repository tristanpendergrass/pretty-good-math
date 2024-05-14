import { Elm } from "./Main.elm";

const app = Elm.Main.init({
  node: document.querySelector("main")
});

// Prevent double tap zoom
let lastTouchEnd = 0;

document.addEventListener('touchend', (event) => {
  const now = (new Date()).getTime();
  if (now - lastTouchEnd <= 300) {
    event.preventDefault();
  }
  lastTouchEnd = now;
}, false);