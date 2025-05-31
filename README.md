Demo at https://www.tristanpendergrass.com/elm-empty-project.

# Development

```
$ npm install
$ npm start -- If on Mac, use npm start-mac
```

# Deployment

## Before first deploy
There's some steps to follow for the first deploy to modify from the elm-empty-project name to the new name:
* In `README.md` modify the url for the demo
* In `package.json` modify parcel:build to have the right url
* In `index.html` modify page title to correct title
* Remove first time deploy section ^

## Deploying to Github pages
* `$ npm run build` or `$ npm run build-mac`: This command builds files in the /docs directory by default
* Push built files to Github
* Log into Github on an account that can edit settings of your project
* Your repo -> Settings -> Pages -> Build and Deployment -> Branch -> master branch, /docs folder

# Defaults
* [Tailwind 3.*](https://tailwindcss.com/) loaded automatically (which includes a css reset)
* [DaisyUI](https://daisyui.com/docs/install/) loaded automatically
* [Feather Icons](https://feathericons.com/) loaded automatically via [elm-feather](https://github.com/feathericons/elm-feather)