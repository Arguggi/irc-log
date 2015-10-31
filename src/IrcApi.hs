{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module IrcApi where

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString            as B
import           Data.Text                  as T
import           Data.Text.Encoding         as TE
import           Data.Time
import qualified Database.PostgreSQL.Simple as PG
import           GHC.Generics
import           Lib
import           Network.Wai
import qualified Network.Wai.Handler.Warp   as W
import           Servant

instance ToJSON PrivMsg

type UserAPI = "api" :> "log" :> Get '[JSON] [PrivMsg]

userAPI :: Proxy UserAPI
userAPI = Proxy

server :: PG.Connection -> Server UserAPI
server conn = liftIO (queryLog conn)

app :: PG.Connection -> Application
app conn  = serve userAPI $ IrcApi.server conn

queryLog :: PG.Connection -> IO [PrivMsg]
queryLog conn = PG.query_ conn "SELECT nick, utctime, message FROM log"

main :: IO ()
main = do
    dbConnection <- PG.connectPostgreSQL "port=5432 user=irc dbname=irc"
    W.run 8645  $ app dbConnection
