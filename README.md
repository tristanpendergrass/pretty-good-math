Demo at https://www.tristanpendergrass.com/pretty-good-math.

# Development

```
$ npm install
$ npm start -- If on Mac, use npm start-mac
```

# Deployment

## Deploying to Github pages
* `$ npm run build` or `$ npm run build-mac`: This command builds files in the /docs directory by default
* Push built files to Github
* Log into Github on an account that can edit settings of your project
* Your repo -> Settings -> Pages -> Build and Deployment -> Branch -> master branch, /docs folder

# Defaults
* [Tailwind 3.*](https://tailwindcss.com/) loaded automatically (which includes a css reset)
* [DaisyUI](https://daisyui.com/docs/install/) loaded automatically
* [Feather Icons](https://feathericons.com/) loaded automatically via [elm-feather](https://github.com/feathericons/elm-feather)

# Todos
[x] Fix drag an drop
[x] Generate answers over time
[x] Move menu buttons from top center to far right or something
[x] Test at screen size 1643px. The "What the heck" is wrapping
[x] Add ability to toggle between addition and multiplication
[x] Add big addition
[x] Add big multiplication
[ ] Update score sheet
  [ ] Remove Count column and show same data in Score column like "Perfect x10"
  [ ] Move Result to top of area
  [ ] Apply fade in animation to final result
[ ] Lock "Big" modes behind passing the normal version first
[ ] Add score system showing your highest earned score
[ ] Store selected game type in local storage
[ ] Initialize seed from random number
[ ] Validate on touch screen
[ ] Playtest