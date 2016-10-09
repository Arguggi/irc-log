{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import           Data.Attoparsec.Text
import qualified Data.ByteString            as B
import           Data.Int
import           Data.Monoid
import           Data.String
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TEnc
import qualified Data.Text.Encoding.Error   as TErr
import qualified Data.Text.IO               as TIO
import           Data.Time
import           Database.PostgreSQL.Simple as PG
import           Lib
import           DB
import qualified Network.Socket             as NS
import qualified Opaleye                    as O
import           System.IO
import           Text.Printf


-- Set up actions to run on start and end, and run the main loop
main :: IO (Bot PG.Connection)
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    bracket connectAll disconnectAll loop
        where
        loop = runReaderT run

connectAll :: IO (Bot PG.Connection)
connectAll = do
    ircHandle <- connectBot
    dbConnection <- PG.connectPostgreSQL "port=5432 user=irc dbname=irc"
    return $ Bot ircHandle dbConnection

disconnectAll :: Bot PG.Connection -> IO ()
disconnectAll x = do
    putStrLn "Closing connection to db and irc network handle"
    hClose . socket $ x
    PG.close . dbConn $ x
    putStrLn "Done"

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
run :: Net PG.Connection (Bot PG.Connection)
run = do
    write "NICK" nick
    write "USER" (nick `mappend` " 0 * :argu-bot")
    write "JOIN" chan
    asks socket >>= listen

showTime :: UTCTime -> T.Text
showTime x  = T.pack (show x)

-- Process each line from the server
listen :: Handle -> Net PG.Connection (Bot PG.Connection)
listen h = forever $ do
    s <- io $ fmap (TEnc.decodeUtf8With TErr.lenientDecode) (B.hGetLine h)
    time <- io getCurrentTime
    io (TIO.putStrLn (showTime time <-> "~" <-> s))
    if ping s then pong s
        else either ignore saveDb $ parseOnly (ircParser time) s
    where
        ignore _ = return ()

ping :: T.Text -> Bool
ping x = "PING :" `T.isPrefixOf` x

pong :: T.Text -> Net PG.Connection ()
pong x = write "PONG" (':' `T.cons` T.drop 6 x)

-- Parse Irc message:
-- :NICKNAME!~NICKNAME@HOST PRIVMSG #haskell.it :MESSAGE
ircParser :: UTCTime -> Parser PrivMsg
ircParser time = PrivMsg <$> ircNickname <*> pure time <*> ircMessage
    where
        ircNickname = char ':' *> takeTill (== '!')
        -- Only save PRIVMSGs
        checkPriv = skipWhile (/= ' ') <* string " PRIVMSG "
        -- sent to the channel we connect to (so this ignores PRIVMSGs to the bot)
        skipChan = string chan <* skipWhile (/= ':') <* char ':'
        ircMessage = checkPriv *> skipChan *> takeText

insertMessage :: Connection -> PrivMsg -> IO Int64
insertMessage conn (PrivMsg n t m) =
    O.runInsert
        conn
        logTable
        (SqlPrivMsg Nothing (O.pgStrictText n) (O.pgUTCTime t) (O.pgStrictText m))

saveDb :: PrivMsg -> Net PG.Connection ()
saveDb x = do
    conn <- asks dbConn
    rows <- io $ insertMessage conn x
    when (rows == 0) $ io $ print ("Message not added" :: T.Text)
    return ()

-- Send a message out to the server we're currently connected to
write :: T.Text -> T.Text -> Net PG.Connection ()
write s t = do
    h <- asks socket
    time <- io getCurrentTime
    io $ TIO.hPutStr h (s <-> t <> "\r\n")
    io $ TIO.putStrLn (showTime time <-> "~" <-> s <-> t)

-- Append with extra space
(<->) :: (Data.String.IsString m, Monoid m) => m -> m -> m
a <-> b = a <> " " <> b

-- Convenience.
io :: IO a -> Net PG.Connection a
io = liftIO
