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
    UTCTime day time <- getCurrentTime
    -- get last day by default
    let yesterday = UTCTime (addDays (-1) day) time
        start = fromMaybe yesterday from
        end = fromMaybe (UTCTime day time) to
    messages <- runCodeQuery conn (allMessagesBetween start end)
    return $ fmap toPrivMsg messages

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

toOrder :: SqlPrivMsg a b (O.Column O.PGTimestamptz) d -> O.Column O.PGTimestamptz
toOrder (SqlPrivMsg _ _ time _ ) = time

-- | Tell ghc the type of the returned values
runCodeQuery
    :: PG.Connection
    -> O.Query (O.Column O.PGText, O.Column O.PGTimestamptz, O.Column O.PGText)
    -> IO [(T.Text, UTCTime, T.Text)]
runCodeQuery = O.runQuery
