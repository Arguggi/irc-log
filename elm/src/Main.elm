module Main where

import Debug
import Html exposing (..)
import Html.Attributes exposing (class)
import Http
import Json.Decode exposing ((:=))
import Task exposing (Task, andThen)

-- MODEL

type alias Message =
    { nickname : String
    , message : String
    , timestamp : String
    }

type alias Model = List Message

init : Model
init = []

-- UPDATE

type Action =
     NoOp
    | SetMessages (List Message)

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model
    SetMessages model' -> model'

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

view : Model -> Html
view model =
  let th' field = th [] [text field]
      tr' message = tr [] [ td [] [text <| message.nickname]
                         , td [] [text <| message.timestamp]
                         , td [] [text <| message.message]
                         ]
  in
    div [class "container"]
    [ table [class "logtable"]
      [ thead [] [tr [] (List.map th' ["Nickname", "Timestamp", "Message"])]
      , tbody [] (List.map tr' model)
      ]
    ]

model : Signal Model
model = Signal.foldp update init actions.signal

main : Signal Html
main = Signal.map view model

message : Json.Decode.Decoder Message
message = Json.Decode.object3 Message
    ("nickname" := Json.Decode.string)
    ("message" := Json.Decode.string)
    ("timestamp" := Json.Decode.string)

get : Task Http.Error (List Message)
get = Http.get (Json.Decode.list message) "http://localhost:8081/log"

port runner : Task Http.Error ()
port runner = get `andThen` (SetMessages >> Signal.send actions.address)
