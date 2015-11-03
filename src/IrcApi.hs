{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module IrcApi where

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString            as B
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Data.Time
import qualified Database.PostgreSQL.Simple as PG
import           GHC.Generics
import           Lib
import           Network.Wai
import qualified Network.Wai.Handler.Warp   as W
import           Servant


instance FromText UTCTime where
    fromText = parseTimeM False defaultTimeLocale "%F" . T.unpack

type UserAPI = "api" :> "log" :> QueryParam "from" UTCTime :> QueryParam "to" UTCTime :> Get '[JSON] [PrivMsg]
    :<|> Raw

userAPI :: Proxy UserAPI
userAPI = Proxy

server :: PG.Connection -> Server UserAPI
server conn = (\x y -> liftIO $ queryLog conn x y)
    :<|> serveDirectory "elm/output"

app :: PG.Connection -> Application
app conn  = serve userAPI $ IrcApi.server conn

queryLog :: PG.Connection -> Maybe UTCTime -> Maybe UTCTime -> IO [PrivMsg]
queryLog conn from to = do
    UTCTime day time <- getCurrentTime
    -- get last day by default
    let yesterday = UTCTime (addDays (-1) day) time
        start = fromMaybe yesterday from
        end = fromMaybe (UTCTime day time) to
    filterDateQuery conn start end

filterDateQuery :: PG.Connection ->  UTCTime -> UTCTime -> IO [PrivMsg]
filterDateQuery conn from to  = PG.query conn
    "SELECT nick, utctime, message FROM log WHERE log.utctime > ? AND log.utctime < ?"
    (from, to)

main :: IO ()
main = do
    dbConnection <- PG.connectPostgreSQL "port=5432 user=irc dbname=irc"
    print "Listening on port 8645"
    W.run 8645  $ app dbConnection
