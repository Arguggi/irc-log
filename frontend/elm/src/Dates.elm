module Dates where

import String exposing (..)

safeDate : String -> String
safeDate date = if validDate date then date else ""

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
