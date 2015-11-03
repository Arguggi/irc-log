{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Text                          as T
import           Data.Time
import           Database.PostgreSQL.Simple         as PG
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           System.IO

server :: String
server = "irc.freenode.org"

chan, nick :: T.Text
chan   = "#haskell.it"
nick   = "argu-bot"

port :: Int
port = 6667

-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net = ReaderT Bot IO
data Bot = Bot
    { socket :: Handle
    , dbConn :: PG.Connection
    }

data PrivMsg = PrivMsg
    { nickname  :: T.Text
    , timestamp :: UTCTime
    , message   :: T.Text
    } deriving (Show)

instance FromRow PrivMsg where
    fromRow = PrivMsg <$> field <*> field <*> field

instance ToRow PrivMsg where
  toRow (PrivMsg n t m) = [toField n, toField t, toField m]

instance ToJSON PrivMsg where
    toJSON (PrivMsg n t m) = object
        [ "nickname" .= n
        , "timestamp" .= utcToText t
        , "message" .= m
        ]

utcToText :: UTCTime -> T.Text
utcToText = T.pack . formatTime defaultTimeLocale "%F %T"
