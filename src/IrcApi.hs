{-# LANGUAGE Arrows            #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module IrcApi where

import           Control.Arrow                      (returnA)
import           Control.Monad.IO.Class
import           Data.Maybe
import qualified Data.Text                  as T
import           Data.Time
import qualified Database.PostgreSQL.Simple as PG
import           GHC.Generics
import           Lib
import           Network.Wai
import qualified Network.Wai.Handler.Warp   as W
import qualified Opaleye                    as O
import           Servant

instance FromText UTCTime where
    fromText = parseTimeM False defaultTimeLocale "%F" . T.unpack

type UserAPI
    =   "api"
        :> "log"
        :> QueryParam "from" UTCTime
        :> QueryParam "to" UTCTime
        :> Get '[JSON] [PrivMsg]
    :<|>
        Raw

userAPI :: Proxy UserAPI
userAPI = Proxy

server :: PG.Connection -> Server UserAPI
server conn = (\x y -> liftIO $ queryLog conn x y)
    :<|> serveDirectory "elm/output"

app :: PG.Connection -> Application
app conn  = serve userAPI $ IrcApi.server conn

queryLog :: PG.Connection -> Maybe UTCTime -> Maybe UTCTime -> IO [PrivMsg]
queryLog conn from to = do
    currentTime <- getCurrentTime
    -- get last day by default
    let (start, end) = dateRange currentTime from to
    messages <- runCodeQuery conn (allMessagesBetween start end)
    return $ fmap toPrivMsg messages

maxDays :: Int
maxDays = 3

-- | * Maximum date range should be 'maxDays' days
--   * If no to or from date is present we want (now - 1, now): just the last day.
--   * If a start date is present we want (start, min end (start + 3).
--   * If only an end date is present we want (end - 3, end).
dateRange :: UTCTime -> Maybe UTCTime -> Maybe UTCTime -> (UTCTime, UTCTime)
dateRange now Nothing     Nothing   = (addDaysUTC (-1) now, now)
dateRange _   Nothing     (Just to) = (addDaysUTC (-3) to, to)
dateRange _   (Just from) Nothing   = (from, addDaysUTC 3 from)
dateRange _   (Just from) (Just to) = (from, min (addDaysUTC 3 from) to)

addDaysUTC :: Integer -> UTCTime -> UTCTime
addDaysUTC n (UTCTime day time) = UTCTime (addDays n day) time

-- | Convert data received from 'allMessagesBetween' and 'PrivMsg'
toPrivMsg :: (T.Text, UTCTime, T.Text) -> PrivMsg
toPrivMsg (a, b, c) = PrivMsg a b c

main :: IO ()
main = do
    dbConnection <- PG.connectPostgreSQL "port=5432 user=irc dbname=irc"
    putStrLn "Listening on port 8645"
    W.run 8645 $ app dbConnection

allMessagesBetween :: UTCTime -> UTCTime -> O.Query (O.Column O.PGText, O.Column O.PGTimestamptz, O.Column O.PGText)
allMessagesBetween start end = proc () -> do
    (SqlPrivMsg _ nickname timestamp mess) <- O.orderBy (O.asc messageTime) (O.queryTable logTable) -< ()
    O.restrict -< (timestamp O..>= O.pgUTCTime start) O..&& (timestamp O..<= O.pgUTCTime end)
    returnA -< (nickname, timestamp, mess)

-- | Tell ghc the type of the returned values
runCodeQuery
    :: PG.Connection
    -> O.Query (O.Column O.PGText, O.Column O.PGTimestamptz, O.Column O.PGText)
    -> IO [(T.Text, UTCTime, T.Text)]
runCodeQuery = O.runQuery
