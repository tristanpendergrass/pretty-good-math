import { Elm } from "./Main.elm";

function getRandomInt() {
  return Math.floor(Math.random() * 999999);
}

const highScoresKey = 'pretty-good-math:highScores'

const defaultScores = {addition: null, additionBig: null, multiplication: null, multiplicationBig: null};
const storedScores = localStorage.getItem(highScoresKey);
console.log('storedScores', storedScores);

let initialScores;

if (storedScores) {
  try {
    initialScores = JSON.parse(storedScores);
  } catch (e) {
    console.error('Error parsing stored scores', e);
    initialScores = defaultScores;
  }
} else {
  initialScores = defaultScores;
}
console.log('initialScores', initialScores);

const app = Elm.Main.init({
  node: document.querySelector('main'),
  flags: {initialSeed: getRandomInt(), highScores: initialScores},
});

app.ports.saveScores.subscribe(function(scores) {
  localStorage.setItem(highScoresKey, JSON.stringify(scores));
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

// Prevents a laptop track pad zoom-pinch being interpreted as CTRL/wheel event
// that will zoom the entire viewport.
// Google maps uses this technique. The viewport can still be scaled with CTRL/+ and
// CTRL/-
window.addEventListener('wheel', event => {
    const { ctrlKey } = event;
    if (ctrlKey) {
        event.preventDefault();
        return;
    }
}, { passive: false });