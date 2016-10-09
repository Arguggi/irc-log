{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module DB where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.Profunctor.Product.TH         (makeAdaptorAndInstance)
import qualified Data.Text                          as T
import           Data.Time
import qualified Opaleye                            as O
import           System.IO
import           Lib

data SqlPrivMsg a b c d
    = SqlPrivMsg
        { messageId   :: a
        , messageNick :: b
        , messageTime :: c
        , messageText :: d
        }

$(makeAdaptorAndInstance "pSqlPrivMsg" ''SqlPrivMsg)

type SqlPrivMsgWrite
    = SqlPrivMsg
          (Maybe (O.Column O.PGInt4))
          (O.Column O.PGText)
          (O.Column O.PGTimestamptz)
          (O.Column O.PGText)

type SqlPrivMsgRead
    = SqlPrivMsg
          (O.Column O.PGInt4)
          (O.Column O.PGText)
          (O.Column O.PGTimestamptz)
          (O.Column O.PGText)

logTable :: O.Table SqlPrivMsgWrite SqlPrivMsgRead
logTable = O.Table "log" (pSqlPrivMsg SqlPrivMsg { messageId = O.optional "id"
                                                 , messageNick = O.required "nick"
                                                 , messageTime = O.required "utctime"
                                                 , messageText = O.required "message" })
