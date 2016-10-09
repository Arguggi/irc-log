{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Maybe
import Data.Foldable
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Generics
import Control.Monad.IO.Class
import Reflex (holdDyn)
import Reflex.Dom hiding ((^.), (.~))
import Reflex.Dom.Class
import Reflex.Dom.Xhr (performRequestAsync, xhrRequest, decodeXhrResponse)
import Reflex.Class (tag, constant)
import Data.Default (def)
import Lib
import Control.Lens
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format

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
nullState newStatus = State Nothing Nothing Nothing Normal None newStatus
