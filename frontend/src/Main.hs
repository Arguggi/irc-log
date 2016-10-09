{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.FileEmbed
import Data.Maybe
import Data.Foldable
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Generics
import Control.Monad.IO.Class
import Reflex (holdDyn)
import Reflex.Dom hiding ((^.), (.~))
import Reflex.Dom.Class
import Reflex.Dom.Xhr (performRequestAsync, xhrRequest, decodeXhrResponse)
import Reflex.Class (tag, constant)
import Data.Default (def)
import Lib
import Types
import Lucid
import Control.Lens
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format

toQuery :: T.Text -> T.Text -> QueryDates
toQuery from to =
    let validF = validDate from
        validT = validDate to
    in case (validF, validT) of
        -- The dates can either be Valid, Invalid or Empty
        -- When one date is empty or invalid and the other is valid
        -- we shouldn't keep fetching data for the Valid when the invalid/empty
        -- one changes
        (ValidDate, EmptyField) -> From from
        (EmptyField, ValidDate) -> To to
        (ValidDate, ValidDate ) -> Both from to
        _                       -> Neither

validDate :: T.Text -> InputCase
validDate date
    | T.null date = EmptyField
    | isJust $ (parseTimeM False defaultTimeLocale "%F" (T.unpack date) :: Maybe UTCTime) = ValidDate
    | otherwise = InvalidDate

main :: IO ()
main = mainWidgetWithCss $(embedFile "style.css") bodyWidget

bodyWidget :: MonadWidget t m => m ()
bodyWidget = elClass "div" "container" $ do
    (fromInput, toInput, showReverse, showToday) <- elClass "div" "input-container" $ do
        fI <- textInput def
        tI <- textInput def
        sR <- checkbox False def
        sT <- checkbox False def
        return (fI, tI, sR, sT)
    let zippedDyn = updated $ zipDynWith toQuery (_textInput_value fromInput) (_textInput_value toInput)
    today <- liftIO getCurrentTime
    asyncRequest <- performRequestAsync $ fforMaybe zippedDyn changeInput
    rec state <- foldDyn updateState initialState updates
        let updates = leftmost
                -- Http Request
                [ xhrToCommand <$> asyncRequest
                -- "Reverse Messages" checkbox
                , const ReverseMessages <$> _checkbox_change showReverse
                -- "Only show today" checkbox
                , decideFilter today <$> updated (_checkbox_value showToday)
                -- We start fetching the data
                , const (SetStatus Loading) <$> fforMaybe zippedDyn changeInput
                ]

        elDynHtmlAttr' "div" ("class" =: "data-container") (stateToHtml <$> state)
    return ()

changeInput :: QueryDates -> Maybe (XhrRequest ())
changeInput Neither = Nothing
changeInput notNull = Just finalRequest
    where
        finalParams = toParams notNull
        finalRequest = xhrRequest "GET" ("http://localhost:8645/api/log" <> finalParams) def

toParams :: QueryDates -> T.Text
toParams (From from) = "?from=" <> from
toParams (To to) = "?to=" <> to
toParams (Both from to) = "?from=" <> from <> "&" <> "to=" <> to

stateToHtml :: State -> T.Text
stateToHtml localState = TL.toStrict . renderText $ case _httpStatus localState of
    Ok -> do
        p_ [class_ "statusMessage"] $ toHtml (showStatus . _httpStatus $ localState)
        p_ [class_ "fromDate"] $ toHtml (maybe invalidDate (showDate . runFD) $ _fromDate localState)
        p_ [class_ "toDate"] $ toHtml (maybe invalidDate (showDate . runTD) $ _toDate localState)
        let logRows = fromMaybe [] (_showingMessages localState)
        table_ [class_ "logtable"] $ do
            thead_ [class_ "loghead"] $
                tr_ $ do
                    th_ "Timestamp (UTC)"
                    th_ "Nickname"
                    th_ "Message"
            tbody_ [class_ "logbody"] $ foldMap toRow logRows
    x -> p_ (toHtml . showStatus $ x)

toRow :: PrivMsg -> Html ()
toRow msg = tr_ [class_ "logrow"] $ do
    td_ (toHtml . showDate $ timestamp msg)
    td_ (toHtml $ nickname msg)
    td_ (toHtml $ message msg)

showDate :: UTCTime -> T.Text
showDate = T.pack . formatTime defaultTimeLocale "%F %T"

invalidDate :: T.Text
invalidDate = "Invalid Date"

showStatus :: Status -> T.Text
showStatus Loading = "Loading"
showStatus Lib.Invalid = "Invalid date"
showStatus ServerError = "Error while fetching data"
showStatus Ok = "Loaded"


-- Decide the command when toggling the "OnlyToday" checkbox
-- If it's True we want to filter the messages, otherwise we should show every
-- message
decideFilter :: UTCTime -> Bool -> Command
decideFilter now True = FilterToday now
decideFilter _ False = ShowAll

-- Try and parse the response
xhrToCommand :: XhrResponse -> Command
xhrToCommand response = fromMaybe (SetStatus Lib.Invalid) $ UpdateMessages <$> decodeXhrResponse response

-- Update the state according to the command
updateState :: Command -> State -> State
updateState cmd oldState = case cmd of
    SetStatus newStatus -> oldState & httpStatus .~ newStatus
    UpdateMessages response -> updateStateResponse response
    ReverseMessages -> oldState & showingMessages %~ fmap reverse
    FilterToday today -> oldState & showingMessages .~ filterToday today (_allMessages oldState)
    ShowAll -> oldState & showingMessages .~ (_allMessages oldState)

updateStateResponse :: LogResponse -> State
updateStateResponse (LogResponse stat fromD toD mess) = case stat of
    Ok -> State (Just mess) (Just mess) (Just (FD fromD)) (Just (TD toD)) Ok
    ServerError -> nullState ServerError
    Lib.Invalid -> nullState Lib.Invalid
    Loading -> initialState

filterToday :: UTCTime -> Maybe [PrivMsg] -> Maybe [PrivMsg]
filterToday today = fmap (filter (isToday today))

-- Is the PrivMsg the same day as the utctime?
isToday :: UTCTime -> PrivMsg -> Bool
isToday today msg = diffDays (utctDay today) (utctDay . timestamp $ msg) < 1
