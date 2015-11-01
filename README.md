# Haskell.it irc bot

The bot needs a postgresql database listening on localhost:5432 with a irc user (no pass) and
a database called irc. You can find a dump in `db/`

## Components

- A bot that connects to an irc channel and saves every message in postgres
- A servant api
- An elm frontend that queries the api and displays the data

## Todo

- Better timestamps
- Better table css
- Fix crash on non UTF-8 irc messages
- Colors in log?
- Better DB table structure?
- D3 graphs?

## Pull requests

Pull requests are always welcome
