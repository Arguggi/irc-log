{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric         #-}

module Lib where

import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Text                          as T
import           Data.Time
import           GHC.Generics
import           System.IO

server :: String
server = "irc.freenode.org"

chan, nick :: T.Text
chan   = "#haskell.it"
nick   = "argu-bot"

port :: Int
port = 6667

data Status
    = Loading
    | Ok
    | Invalid
    | ServerError
    deriving (Generic, Show)

-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net a = ReaderT (Bot a) IO
data Bot a
    = Bot
        { socket :: Handle
        , dbConn :: a
        }

data PrivMsg
    = PrivMsg
        { nickname  :: T.Text
        , timestamp :: UTCTime
        , message   :: T.Text
        } deriving (Generic, Show)

data LogResponse
    = LogResponse
        { status   :: Status
        , fromDate :: UTCTime
        , toDate   :: UTCTime
        , messages :: [PrivMsg]
        } deriving (Generic, Show)

instance ToJSON PrivMsg
instance ToJSON LogResponse
instance ToJSON Status
instance FromJSON PrivMsg
instance FromJSON LogResponse
instance FromJSON Status
