# servant-elm-template

## Description
* This is a template for using servant and elm
* This template is built with:
    * Stack -- build the haskell project
    * Postgres -- database
    * Elm -- front end
    * Fusebox -- bundling tool
    * Typescript -- for frontend javascript such as embedding your elm app and ports
    * Ruby -- for database migrations and build tool
    * npm -- node package manager
* This app is just a raw skeleton to use haskell for your backend and elm for your front end.
* The example posts a dice to the server, rolls that dice, then sends back the result
* The code-generator generates your elm types, decoders, encoders, and api endpoints from your haskell types

## Prerequisites
1. Install [NodeJS v6.9+](https://nodejs.org/en/download/current/) `choco install nodejs-lts`
2. Install [Elm v0.18](https://guide.elm-lang.org/install.html) `choco install elm-platform`
3. Install [Ruby](https://www.ruby-lang.org/en/documentation/installation/) `choco install ruby`
4. Install [Postgres](https://www.postgresql.org/) `choco install postgresql`
5. Install [Stack](https://docs.haskellstack.org/en/stable/README/) `choco install haskell-stack`
6. Run stack setup `stack setup`
7. Install hpack `stack install hpack`
8. Install intero `stack install intero`
9. Install typescript globally `npm install -g typescript`
10. Install fusebox globally `npm install -g fuse-box`
11. Run `npm install`
12. Install pg `gem install pg`
13. Install standalone_migrations `gem install standalone_migrations`

## Development
* run `rake`
    * this will:
        * build your haskell project using `stack build`
        * generate your elm code from your haskell code using `stack exec code-generator`
        * build your elm project using `elm-make`
        * bundle your project and put it into the dist folder using `node fuse copy && node fuse`
        * run your project using `stack exec app`
* IDE
   * I recommend using VS Code with the following plugins:
      * language-haskell
      * Vans.haskero
      * sbrink.elm
      * abadi199.elm-format
      * realazy.elmx
      * monofon.hindent-format
      * rebornix.ruby
      * groksrc.ruby
      * hoovercj.ruby-linter
      * mrmInc.vscode-scss
      * emmanuelbeziat.vscode-great-icons
      * joeandaverde.vscode-elm-jump
      * siegebell.prettify-symbols-mode

## Included Already
* JQuery
* Bootstrap
* Font Awesome
* Tether

## Project Structure
* `api` -- parent folder for your api type
    * `Api` -- all the files that go to your api
        * `Module`
            * `Handler` -- handler for your module
            * `Helper` -- helper for your module
        * `Helper.hs` -- helper file for your server
        * `Types.hs` -- types file for your server
        * `Server.hs` -- your server that you will use to serve your backend
* `app` -- parent folder for your app
    * `Main.hs` -- main file for your backend.  This is where you will run the server
* `code-generator` -- parent folder for the code-generator project
    * `Main.hs` -- main file for the code-generator.  This is where you put everything you want to generate elm code for.
* `db` -- parent folder for your db migrations
    * `migrate` -- holds all of your migration scripts
    * `config.yml` your db config for migrations
* `frontend` -- parent folder for all of your frontend code
    * `dist` -- this is where the bundle outputs your compiled code
    * `elm` -- your elm project
        * `Shared`
            * `Generated.elm` -- this is the generated elm file from the code-generator
    * `static` -- all of your static assets needed for your frontend
        * `lib` -- your downloaded libraries
            * `scss` -- all of your stylesheets
        * `index.html` -- this is your index.html file where your elm app is embedded
        * `index.ts` -- this is your frontend javascript code such as ports etc
    * `tests` -- your elm test directory
    * `fues.js` -- the fusebox script for bundling the project
* `test` -- your haskell test directory
* `package.yaml` -- used to manage all of your haskell packages
* `Rakefile` -- used to build the project
