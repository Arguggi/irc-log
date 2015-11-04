{-# LANGUAGE OverloadedStrings #-}

module IrcLog where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import           Data.Attoparsec.Text
import qualified Data.ByteString            as B
import           Data.Int
import           Data.Monoid
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TEnc
import qualified Data.Text.Encoding.Error   as TErr
import qualified Data.Text.IO               as TIO
import           Data.Time
import           Data.String
import           Database.PostgreSQL.Simple as PG
import           Lib
import qualified Network.Socket             as NS
import           System.IO
import           Text.Printf

-- Set up actions to run on start and end, and run the main loop
main :: IO Bot
main = bracket connectAll disconnectAll loop
  where
    loop = runReaderT run

connectAll :: IO Bot
connectAll = do
    ircHandle <- connectBot
    dbConnection <- PG.connectPostgreSQL "port=5432 user=irc dbname=irc"
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
    s <- io $ liftM (TEnc.decodeUtf8With TErr.lenientDecode) (B.hGetLine h)
    io (TIO.putStrLn s)
    time <- io getCurrentTime
    if ping s then pong s
        else either ignore saveDb $ parseOnly (ircParser time) s
    where
        ignore _ = return ()

ping :: T.Text -> Bool
ping x = "PING :" `T.isPrefixOf` x

pong :: T.Text -> Net ()
pong x = write "PONG" (':' `T.cons` T.drop 6 x)

ircParser :: UTCTime -> Parser PrivMsg
ircParser time = do
    nickN <- char ':' *> takeTill (== '!')
    skipWhile (/= ' ') <* string " PRIVMSG "
    _ <- string chan <* skipWhile (/= ':') <* char ':'
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

-- Send a message out to the server we're currently connected to
write :: T.Text -> T.Text -> Net ()
write s t = do
    h <- asks socket
    io $ TIO.hPutStr h (s <-> t <> "\r\n")
    io $ TIO.putStrLn (s <-> t)

-- Append with extra space
(<->) :: (Data.String.IsString m, Monoid m) => m -> m -> m
a <-> b = a <> " " <> b

-- Convenience.
io :: IO a -> Net a
io = liftIO
