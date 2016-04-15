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
    let yesterday = addDaysUTC (-1) currentTime
        start = fromMaybe yesterday from
        end = fromMaybe currentTime to
        -- Only get max 10 days of logs at once
        actualEnd = min tenDaysAfter end
                    where tenDaysAfter = addDaysUTC 3 start
    messages <- runCodeQuery conn (allMessagesBetween start actualEnd)
    return $ fmap toPrivMsg messages

addDaysUTC :: Integer -> UTCTime -> UTCTime
addDaysUTC n (UTCTime day time) = UTCTime (addDays n day) time


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
