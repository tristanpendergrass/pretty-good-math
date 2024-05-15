import { Elm } from "./Main.elm";

function getRandomInt() {
  return Math.floor(Math.random() * 999999);
}

const storedScores = localStorage.getItem('highScores');
const initialScores = storedScores ? JSON.parse(storedScores) : {addition: null, additionBig: null, multiplication: null, multiplicationBig: null};

const app = Elm.Main.init({
  node: document.querySelector('main'),
  flags: {initialSeed: getRandomInt(), highScores: initialScores},
});

app.ports.saveScores.subscribe(function(scores) {
  console.log('foobar saving scores', scores)
  localStorage.setItem('highScores', JSON.stringify(scores));
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