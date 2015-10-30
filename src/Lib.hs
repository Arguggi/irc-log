{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import           Data.Attoparsec.Text
import           Data.Int
import qualified Data.Text                          as T
import qualified Data.Text.IO                       as TIO
import           Data.Time
import           Database.PostgreSQL.Simple         as PG
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import qualified Network.Socket                     as NS
import           System.IO
import           Text.Printf

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


-- Set up actions to run on start and end, and run the main loop
main :: IO Bot
main = bracket connectAll disconnectAll loop
  where
    loop = runReaderT run

connectAll :: IO Bot
connectAll = do
    ircHandle <- connectBot
    dbConnection <- PG.connectPostgreSQL "host=localhost port=5432 user=irc dbname=irc"
    return $ Bot ircHandle dbConnection

disconnectAll :: Bot -> IO ()
disconnectAll x = do
    hClose . socket $ x
    PG.close . dbConn $ x


-- Connect to the server and return the initial bot state
connectBot :: IO Handle
connectBot = notify $ do
    addrInfo <- NS.getAddrInfo Nothing (Just server) (Just $ show port)
    let serverAddr = head addrInfo
    sock <- NS.socket (NS.addrFamily serverAddr) NS.Stream NS.defaultProtocol
    NS.connect sock (NS.addrAddress serverAddr)
    irchandle <- NS.socketToHandle sock ReadWriteMode
    hSetBuffering irchandle NoBuffering
    hSetEncoding irchandle utf8
    return irchandle
  where
    notify = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")

-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
run :: Net Bot
run = do
    write "NICK" nick
    write "USER" (nick `mappend` " 0 * :tutorial bot")
    write "JOIN" chan
    asks socket >>= listen

-- Process each line from the server
listen :: Handle -> Net Bot
listen h = forever $ do
    s <- io (TIO.hGetLine h)
    io (TIO.putStrLn s)
    time <- io getCurrentTime
    if ping s then pong s
        else case parseOnly (ircParser time) s of
            Left _ -> return ()
            Right ircMessage -> saveDb ircMessage

ping :: T.Text -> Bool
ping x = "PING :" `T.isPrefixOf` x

pong :: T.Text -> Net ()
pong x = write "PONG" (':' `T.cons` T.drop 6 x)

ircParser :: UTCTime -> Parser PrivMsg
ircParser time = do
    _ <- char ':'
    nickN <- takeTill (== '!')
    skipWhile (/= ' ')
    _ <- char ' '
    _ <- string "PRIVMSG"
    skipWhile (/= ':')
    _ <- char ':'
    mess <- takeText
    return $ PrivMsg nickN time (T.init mess)

insertMessage :: Connection -> PrivMsg -> IO Int64
insertMessage conn = execute conn "INSERT INTO log (nick, utctime, message) values (?,?,?)"

saveDb :: PrivMsg -> Net ()
saveDb x = do
    conn <- asks dbConn
    rows <- io $ insertMessage conn x
    when (rows == 0) $ io $ print ("Message not added" :: T.Text)
    return ()

-- Send a privmsg to the current chan + server
privmsg :: T.Text -> Net ()
privmsg s = write "PRIVMSG" (chan `T.append` " :" `T.append` s)

-- Send a message out to the server we're currently connected to
write :: T.Text -> T.Text -> Net ()
write s t = do
    h <- asks socket
    io $ TIO.hPutStr h (s `T.append` " " `T.append` t `T.append` " " `T.append` "\r\n")
    io $ TIO.putStrLn (s `T.append` " " `T.append` t)

-- Convenience.
io :: IO a -> Net a
io = liftIO
