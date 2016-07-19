{-# LANGUAGE Arrows            #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Arrow                      (returnA)
import           Control.Monad.IO.Class
import qualified Data.Text                  as T
import           Data.Time
import qualified Database.PostgreSQL.Simple as PG
import           Lib
import           Network.Wai
import qualified Network.Wai.Handler.Warp   as W
import qualified Opaleye                    as O
import           Servant
import           Web.HttpApiData            ()


-- The parseQueryParam instance for UTCTime expects a Text with format
-- "%H:%M:%SZ" but since we only want to query for days we have to change the instance
-- with the newtype
newtype MyDay = MyDay { unDay :: UTCTime }

instance FromHttpApiData MyDay where
    parseQueryParam x = MyDay <$> parseUTC x
        where parseUTC :: T.Text -> Either T.Text UTCTime
              parseUTC = parseTimeM False defaultTimeLocale "%F" . T.unpack

type UserAPI
    =   "api"
        :> "log"
        :> QueryParam "from" MyDay
        :> QueryParam "to" MyDay
        :> Get '[JSON] LogResponse
    :<|>
        Raw

userAPI :: Proxy UserAPI
userAPI = Proxy

apiServer :: PG.Connection -> Server UserAPI
apiServer conn = (\x y -> liftIO $ queryLog conn x y)
    :<|> serveDirectory "elm/output"

app :: PG.Connection -> Application
app conn  = serve userAPI $ apiServer conn

queryLog :: PG.Connection -> Maybe MyDay -> Maybe MyDay -> IO LogResponse
queryLog conn fromD toD = do
    currentTime <- getCurrentTime
        -- Unwrap the newtype
    let from = fmap unDay fromD
        to = fmap unDay toD
        -- get last day by default
        (start, end) = dateRange currentTime from to
    sqlMessages <- runCodeQuery conn (allMessagesBetween start end)
    let messageList = fmap toPrivMsg sqlMessages
    return $ LogResponse 0 start end messageList

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
    (SqlPrivMsg _ nicknameP timestampP messP) <- O.orderBy (O.asc messageTime) (O.queryTable logTable) -< ()
    O.restrict -< (timestampP O..>= O.pgUTCTime start) O..&& (timestampP O..<= O.pgUTCTime end)
    returnA -< (nicknameP, timestampP, messP)

-- | Tell ghc the type of the returned values
runCodeQuery
    :: PG.Connection
    -> O.Query (O.Column O.PGText, O.Column O.PGTimestamptz, O.Column O.PGText)
    -> IO [(T.Text, UTCTime, T.Text)]
runCodeQuery = O.runQuery
