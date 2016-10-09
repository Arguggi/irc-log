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

data Command
    = SetStatus Status
    | UpdateMessages LogResponse
    | ReverseMessages
    | FilterToday UTCTime
    | ShowAll
    deriving (Show)

data State
    = State
    { _showingMessages :: Maybe [PrivMsg]
    , _allMessages :: Maybe [PrivMsg]
    , _fromDate :: Maybe FromDate
    , _toDate :: Maybe ToDate
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
nullState newStatus = State Nothing Nothing Nothing Nothing newStatus
