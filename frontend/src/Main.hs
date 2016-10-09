{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Data.Maybe
import Data.Foldable
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Monad.IO.Class
import Reflex (holdDyn)
import Reflex.Dom
import Reflex.Dom.Class
import Reflex.Dom.Xhr (performRequestAsync, xhrRequest, decodeXhrResponse)
import Reflex.Class (tag, constant)
import Data.Default (def)
import Lib
import Lucid
import Data.Time.Clock
import Data.Time.Format

main :: IO ()
main = mainWidget $ el "div" $ do
    el "p" $ text "Hello"
    t <- textInput def
    dynText $ _textInput_value t
    now <- liftIO getCurrentTime
    buttonEvent <- button "click me"
    let req = xhrRequest "GET" "http://localhost:8645/api/log?from=2016-10-07" def
        parseDef = defResp now
    asyncEvent <- performRequestAsync (tag (constant req) buttonEvent)
    buttonDyn <- holdDyn (Just parseDef) $ fmap decodeXhrResponse asyncEvent
    let htmlMess log = TL.toStrict . renderText $ logHtml log
    elDynHtml' "div" $ fmap htmlMess buttonDyn
    return ()

defResp :: UTCTime -> LogResponse
defResp now = LogResponse Loading now now []

logHtml :: Maybe LogResponse -> Html ()
logHtml (Just (LogResponse status from to messages)) = do
    div_ (showTime from)
    div_ (showTime to)
    div_ (fold $ map (div_ . toHtml . show) messages)
logHtml Nothing = div_ "asd"

showTime :: UTCTime -> Html ()
showTime time = toHtml $ formatTime defaultTimeLocale "%c" time
