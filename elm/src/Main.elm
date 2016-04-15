module Main where

import Effects exposing (..)
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Http
import Json.Decode exposing ((:=))
import Task exposing (Task, andThen)
import StartApp
import String
import Mouse

-- MODEL

type alias Message =
    { nickname : String
    , message : String
    , timestamp : String
    }

type DateField = From | To

type alias Model =
  { messages : List Message
  , fromDate : String
  , toDate   : String
  }


init : (Model, Effects Action)
init = (Model [] "" "", getData "" "")

-- UPDATE

type Action =
     NoOp
    | SetMessages (List Message)
    | UpdateDate String DateField

--update : Action -> Model -> (Model, Effects Action)
update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp                     -> (model, Effects.none)
    SetMessages newMessages  -> ({ model | messages = newMessages } , Effects.none)
    UpdateDate date From     -> ({ model | fromDate = date }, if validDate date then
                                                                getData date model.toDate
                                                              else Effects.none)
    UpdateDate date To       -> ({ model | toDate = date },   if validDate date then
                                                                getData model.fromDate date
                                                              else Effects.none)

-- This will do for now, the api checks for invalid dates anyway
validDate : String -> Bool
validDate x = case String.split "-" x of
  [year, month, day] -> (String.length year == 4) && (String.length month == 2) && (String.length day == 2)
  [] -> True
  _ -> False


view : Signal.Address Action -> Model -> Html
view address model =
  let allMessages = model.messages
      th' field = th [] [text field]
      tr' message = tr [class "logrow"] [ td [] [text <| message.timestamp]
                       , td [] [text <| message.nickname]
                       , td [] [text <| message.message]
                       ]
  in
    div [class "container", containerStyle]
    [ div [class "date-container", dateContainerStyle]
      [ div [class "date", dateStyle]
        [ input
          [ placeholder "From: YYYY-MM-DD"
          , value (model.fromDate)
          , on "input" targetValue (\x -> Signal.message address (UpdateDate x From))
          , myStyle
          ]
          []
        ]
      , div [class "date", dateStyle]
        [ input
          [ placeholder "To: YYYY-MM-DD"
          , value (model.toDate)
          , on "input" targetValue (\x -> Signal.message address (UpdateDate x To))
          , myStyle
          ]
          []
        ]
      ]
      , table [class "logtable", logTableStyle]
        [ thead [class "loghead"] [tr [] (List.map th' ["Timestamp (UTC)", "Nickname", "Message"])]
        , tbody [class "logbody"] (List.map tr' allMessages)
        ]
    ]

logTableStyle : Attribute
logTableStyle = style [ ("border-spacing", "8px") ]

containerStyle : Attribute
containerStyle = style [ ("margin", "auto") ]

dateContainerStyle : Attribute
dateContainerStyle = style [ ("display", "flex")
                           , ("flex-wrap", "wrap")
                           , ("justify-content", "space-around")
                           ]

dateStyle : Attribute
dateStyle = style [ ("padding", "4px") ]

app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , inputs = []
    , update = update
    , view = view
    }

main : Signal Html
main = app.html

message : Json.Decode.Decoder Message
message = Json.Decode.object3 Message
  ("nickname" := Json.Decode.string)
  ("message" := Json.Decode.string)
  ("timestamp" := Json.Decode.string)

getTask : String -> String -> Task Http.Error (List Message)
getTask from to = Http.get (Json.Decode.list message) (Http.url "/api/log/" [("from", from), ("to", to)])

port runner : Signal (Task Never ())
port runner = app.tasks

getData : String -> String -> Effects Action
getData from to = getTask from to
  |> Task.toMaybe
  |> Task.map toAction
  |> Effects.task
--
toAction : Maybe (List Message) -> Action
toAction list = case list of
  Nothing -> NoOp
  (Just mess) -> SetMessages mess

myStyle : Attribute
myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]
