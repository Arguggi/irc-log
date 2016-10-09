module Styles where

import Dates exposing (validDate)
import Html exposing (..)
import Html.Attributes exposing (..)

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
    , ("max-width", "1000px")
    , ("margin" , "auto")
    ]

dateContainerStyle : Attribute
dateContainerStyle =
  style
    [ ("display", "flex")
    , ("flex-wrap", "wrap")
    , ("justify-content", "space-around")
    , ("align-items", "center")
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



headerStyle : Attribute
headerStyle =
  style
    [ ("display", "flex")
    , ("flex-direction", "column")
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
