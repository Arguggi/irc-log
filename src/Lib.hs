{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Monad.Reader
import qualified Data.Text                          as T
import           Data.Time
import           Database.PostgreSQL.Simple         as PG
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics
import           System.IO

server :: String
server = "irc.freenode.org"

chan, nick :: T.Text
chan   = "#haskell.it"
--chan   = "#test-test"
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
    } deriving (Show, Generic)

instance FromRow PrivMsg where
    fromRow = PrivMsg <$> field <*> field <*> field

instance ToRow PrivMsg where
  toRow (PrivMsg n t m) = [toField n, toField t, toField m]
