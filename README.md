# Haskell.it irc bot

The bot needs a `postgresql` database listening on `localhost:5432` with a `irc` user (no pass) and
a database called `irc`. You can find a dump in `backend/db/`

## Build

    $ git clone https://github.com/Arguggi/irc-log.git
    $ cd irc-log/backend
    $ stack build

Once if finishes you should find 2 executables in `.stack-work`:

    - irc-api: Servant api
    - irc-log: The irc bot

If you want to use the irc logger remember to change the `nick` in `Lib.hs`.

## Components

- A bot that connects to an irc channel and saves every message in postgres
- A servant api
- An ghcjs frontend that queries the api and displays the data

## Todo

- Colors in log?
- Better DB table structure?
- D3 graphs?

## Pull requests

Pull requests are always welcome
