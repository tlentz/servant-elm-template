# servant-elm-template

## Description
* This is a template for using servant and elm
* This template is built with:
    * Stack -- build the haskell project
    * Postgres -- database
    * Elm -- front end
    * Webpack -- bundling tool
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
10. Run `npm install`
11. Install pg `gem install pg`
12. Install standalone_migrations `gem install standalone_migrations`

## Development
* cd into `src` and run `rake watch`
    * this will build your project and enable hot reloading
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

## Production
* cd into `src` and run `rake build`
    * this will build your app for production by generating your dist folder
* then run `rake serve` to serve your production app

## Included Already
* JQuery
* Bootstrap
* Font Awesome
* Tether

## Project Structure
* `src` -- all source code goes here
    * `client` -- all client side code
    * `db` -- all database migrations
    * `server` -- all server side code