{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import qualified Data.Text as T
import Lib
import Control.Lens
import Data.Time.Clock

data Order
    = Normal
    | Reversed
    deriving (Show)

data Filter
    = None
    | OnlyToday UTCTime
    deriving (Show)

data InputCase
    = EmptyField
    | ValidDate
    | InvalidDate
    deriving (Show)

data QueryDates
    = Both T.Text T.Text
    | From T.Text
    | To T.Text
    | Neither
    deriving (Show)

data Command
    = SetStatus Status
    | UpdateMessages LogResponse
    | SetOrder Order
    | SetFilter Filter
    deriving (Show)

data State
    = State
    { _messages :: Maybe [PrivMsg]
    , _fromDate :: Maybe FromDate
    , _toDate :: Maybe ToDate
    , _order :: Order
    , _dateFilter :: Filter
    , _httpStatus :: Status
    } deriving (Show)

newtype FromDate = FD { runFD ::  UTCTime } deriving (Show)
newtype ToDate = TD { runTD :: UTCTime } deriving (Show)

errorUpdate :: Command
errorUpdate = SetStatus Lib.Invalid

makeLenses ''State

initialState :: State
initialState = nullState Loading

nullState :: Status -> State
nullState = State Nothing Nothing Nothing Normal None
