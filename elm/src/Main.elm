module Main where

import Dates exposing (..)
import Effects exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Http
import Json.Decode exposing ((:=))
import Task exposing (Task, andThen)
import StartApp
import Styles exposing (..)

-- MODEL

type alias Response =
  { status : Int
  , fromDate : String
  , toDate : String
  , messages : List Message
  }

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
  , fromDateShow : String
  , toDateShow   : String
  , loadingMessage  : String
  }


init : (Model, Effects Action)
init = (Model [] "" "" "" "" "", getData "" "")

-- UPDATE

type Action
  = NoOp
  | SetMessages (List Message) String String
  | SetStatus String
  | UpdateDate DateField String

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp                  -> nofx model
    SetMessages newMessages fromDate toDate -> nofx { model | messages = newMessages
                                                        , loadingMessage = "Messages Loaded"
                                                        , fromDateShow = fromDate
                                                        , toDateShow = toDate }
    SetStatus status      -> nofx { model | loadingMessage = status }
    UpdateDate field date -> dateFormChange model field date

nofx : Model -> (Model, Effects Action)
nofx model = (model, Effects.none)

dateFormChange : Model -> DateField -> String -> (Model, Effects Action)
dateFormChange model field newDate =
  let updatedModel = updateDate model newDate field
      loadingModel = { updatedModel | loadingMessage = "Loading messages"
                                    , fromDateShow = ""
                                    , toDateShow = ""
                                    , messages = []}
      invalidDateModel = { updatedModel | loadingMessage = "Invalid Date" }
  in
    if validDate newDate
      then (loadingModel , (getData (safeDate updatedModel.fromDate) (safeDate updatedModel.toDate)))
      else (invalidDateModel, Effects.none)

updateDate : Model -> String -> DateField -> Model
updateDate model date field =
  case field of
    From -> { model | fromDate = date }
    To   -> { model | toDate   = date }

view : Signal.Address Action -> Model -> Html
view address model =
  let allMessages = model.messages
      th' field = th [] [text field]
      tr' message = tr [class "logrow"] [ td [] [text message.timestamp]
                       , td [] [text message.nickname]
                       , td [] [text message.message]
                       ]
      statusMess txt = p [statusStyle] [text txt]
  in
    div [class "container", containerStyle]
    [ div [class "date-container", dateContainerStyle]
      [ div [dateStyle]
        [ input
          [ placeholder "From: YYYY-MM-DD"
          , value (model.fromDate)
          , on "input" targetValue (UpdateDate From >> Signal.message address)
          , inputStyle model.fromDate
          ]
          []
        ]
      , div [dateStyle]
        [ input
          [ placeholder "To: YYYY-MM-DD"
          , value (model.toDate)
          , on "input" targetValue (UpdateDate To >> Signal.message address)
          , inputStyle model.toDate
          ]
          []
        ]
      ]
      , div [headerStyle]
        (List.map statusMess
          [ model.loadingMessage
          , model.fromDateShow
          , model.toDateShow])
      , table [class "logtable", logTableStyle]
        [ thead [class "loghead"] [tr [] (List.map th' ["Timestamp (UTC)", "Nickname", "Message"])]
        , tbody [class "logbody"] (List.map tr' allMessages)
        ]
    ]

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

logResponse : Json.Decode.Decoder Response
logResponse =
  Json.Decode.object4 Response
    ("status" := Json.Decode.int)
    ("fromDate" := Json.Decode.string)
    ("toDate" := Json.Decode.string)
    ("messages" := Json.Decode.list message)

message : Json.Decode.Decoder Message
message =
  Json.Decode.object3 Message
    ("nickname" := Json.Decode.string)
    ("message" := Json.Decode.string)
    ("timestamp" := Json.Decode.string)

getTask : String -> String -> Task Http.Error Response
getTask from to = Http.get logResponse <| Http.url "/api/log/" [("from", from), ("to", to)]

port runner : Signal (Task Never ())
port runner = app.tasks

getData : String -> String -> Effects Action
getData from to = getTask from to
  |> Task.toResult
  |> Task.map toAction
  |> Effects.task

toAction : Result Http.Error Response -> Action
toAction response =
  let
    requestFailed = SetStatus "Request failed"
  in
    case response of
      Err err -> SetStatus <| "Error: " ++ showHttpError err
      Ok response -> case response.status of
        0 -> SetMessages response.messages response.fromDate response.toDate
        _ -> SetStatus "Api error"

showHttpError : Http.Error -> String
showHttpError err =
  case err of
    Http.Timeout -> "Request timed out"
    Http.NetworkError -> "Can't connect to the server"
    Http.UnexpectedPayload _ -> "Invalid response from api"
    Http.BadResponse code _ -> "Got response code " ++ (toString code)
