{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Lib where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.Profunctor.Product.TH         (makeAdaptorAndInstance)
import qualified Data.Text                          as T
import           Data.Time
import qualified Database.PostgreSQL.Simple         as PG
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import qualified Opaleye                            as O
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
data Bot
    = Bot
        { socket :: Handle
        , dbConn :: PG.Connection
        }

data PrivMsg
    = PrivMsg
        { nickname  :: T.Text
        , timestamp :: UTCTime
        , message   :: T.Text
        } deriving (Show)

data LogResponse
    = LogResponse
        { status :: Int
        , fromDate :: UTCTime
        , toDate :: UTCTime
        , messages :: [PrivMsg]
        }

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

instance ToJSON LogResponse where
    toJSON (LogResponse s f t m) = object
        [ "status" .= s
        , "fromDate" .= utcToText f
        , "toDate" .= utcToText t
        , "messages" .= toJSON m
        ]

utcToText :: UTCTime -> T.Text
utcToText = T.pack . formatTime defaultTimeLocale "%F %T"

data SqlPrivMsg a b c d
    = SqlPrivMsg
        { messageId   :: a
        , messageNick :: b
        , messageTime :: c
        , messageText :: d
        }

$(makeAdaptorAndInstance "pSqlPrivMsg" ''SqlPrivMsg)

type SqlPrivMsgWrite
    = SqlPrivMsg
          (Maybe (O.Column O.PGInt4))
          (O.Column O.PGText)
          (O.Column O.PGTimestamptz)
          (O.Column O.PGText)

type SqlPrivMsgRead
    = SqlPrivMsg
          (O.Column O.PGInt4)
          (O.Column O.PGText)
          (O.Column O.PGTimestamptz)
          (O.Column O.PGText)

logTable :: O.Table SqlPrivMsgWrite SqlPrivMsgRead
logTable = O.Table "log" (pSqlPrivMsg SqlPrivMsg { messageId = O.optional "id"
                                                 , messageNick = O.required "nick"
                                                 , messageTime = O.required "utctime"
                                                 , messageText = O.required "message" })
