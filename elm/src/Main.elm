module Main where

import Effects exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Http
import Json.Decode exposing ((:=))
import Task exposing (Task, andThen)
import StartApp
import String

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

safeDate : String -> String
safeDate date = if validDate date then date else ""

updateDate : Model -> String -> DateField -> Model
updateDate model date field =
  case field of
    From -> { model | fromDate = date }
    To   -> { model | toDate   = date }

-- This will do for now, the api checks for invalid dates anyway
validDate : String -> Bool
validDate x =
  case String.split "-" x of
    [year, month, day] ->
      (String.length year == 4)
      && (String.length month == 2)
      && (String.length day == 2)
    [""] -> True
    _ -> False

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

logTableStyle : Attribute
logTableStyle =
  style
    [ ("border-spacing", "8px")
    , ("align-self", "center")
    ]

containerStyle : Attribute
containerStyle =
  style
    [ ("display", "flex")
    , ("flex-direction", "column")
    ]

dateContainerStyle : Attribute
dateContainerStyle =
  style
    [ ("display", "flex")
    , ("flex-wrap", "wrap")
    , ("justify-content", "space-around")
    ]

dateStyle : Attribute
dateStyle =
  style
    [ ("padding", "4px")
    ]

statusStyle : Attribute
statusStyle =
  style
    [ ("margin", "4px 0 4px 0")
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
  |> Task.toMaybe
  |> Task.map toAction
  |> Effects.task

toAction : Maybe Response -> Action
toAction list =
  let
    requestFailed = SetStatus "Request failed"
  in
    case list of
      Nothing -> requestFailed
      (Just response) -> case response.status of
        0 -> SetMessages response.messages response.fromDate response.toDate
        _ -> requestFailed

headerStyle : Attribute
headerStyle =
  style
    [ ("display", "flex")
    , ("flex-direction", "column")
    , ("justify-content", "center")
    , ("align-items", "center")
    ]

inputStyle : String -> Attribute
inputStyle date =
  let
    boxColor =
      if validDate date
         then lightGreen
         else lightRed
  in
    style
      [ ("padding", "10px 0")
      , ("font-size", "1.5em")
      , ("text-align", "center")
      , ("box-shadow", "0 0 6px " ++ boxColor)
      ]

lightGreen : String
lightGreen = "#79CF6D"

lightRed : String
lightRed = "#CF6D6D"
