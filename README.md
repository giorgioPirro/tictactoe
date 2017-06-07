# Tic Tac Toe

The classic Noughts and Crosses game, written in Elm.

You can play it online [here](http://tictactoeelm.getforge.io/)

Or build it locally following these instructions:

- install elm 0.18 from [this page](https://guide.elm-lang.org/install.html), or [download directly](http://install.elm-lang.org/Elm-Platform-0.18.pkg) if you have a mac.
- from a terminal, clone this repo and move into it: `git clone https://github.com/giorgioPirro/tictactoe.git && cd tictactoe`
- build the app: `elm-make src/Main.elm --output elm.js`
- open the app, either with `open index.html` in terminal or by opening the `index.html` file in a browser

To run the tests:

- install Node.js. You can download it from [here](https://nodejs.org/en/). Node includes npm, which is also required.
- install elm-test in your machine: `npm install -g elm-test` (you might have to prepend `sudo` to this one)
- if you already had elm-test installed, please make sure you are running the latest version (0.18.4) with: `npm update -g elm-test` (you might have to prepend `sudo` to this one too)
- move to the root of this repository
- run the tests: `elm-test`or `elm-test --watch` for continuous testing
