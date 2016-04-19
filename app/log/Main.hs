{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified IrcLog
import qualified Lib
import           System.Remote.Monitoring

main :: IO Lib.Bot
main = do
    _ <- forkServer "localhost" 23453
    IrcLog.main
