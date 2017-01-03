{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Crypto.Hash (hash, MD5)
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Control.Monad.IO.Class
import Control.Monad (when)
import Reflex.Dom hiding ((.~))
import Reflex.Dom.Xhr (performRequestAsync, xhrRequest, decodeXhrResponse)
import Data.Default (def)
import Lib
import Types
import Control.Lens hiding (Reversed, from, to)
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format

main :: IO ()
main = mainWidget bodyWidget

bodyWidget :: MonadWidget t m => m ()
bodyWidget = do
    el "header" $ elClass "h1" "page-title" homePageLink
    elClass "div" "container" $ do
        (fromInput, toInput, showReverse, showToday) <- elClass "div" "input-container" $ do
            (fI, tI) <- elClass "div" "dateinput-container" $
                (,) <$> newDateInput "From: YYYY-MM-DD" <*> newDateInput "To: YYYY-MM-DD"
            (srC, stC) <- elClass "div" "checkbox-container" $
                (,) <$> newCheckBox "Reverse" <*> newCheckBox "Only today"
            return (fI, tI, srC, stC)
        let zippedDyn = updated $ zipDynWith toQuery (_textInput_value fromInput) (_textInput_value toInput)
        asyncRequest <- performRequestAsync $ fforMaybe zippedDyn changeInput
        post <- getPostBuild
        initialRequest <- performRequestAsync (const (apiRequest "") <$> post)
        today <- liftIO getCurrentTime
        rec state <- foldDyn updateState initialState updates
            let updates = leftmost
                    -- Http Request from date inputs
                    [ xhrToCommand <$> asyncRequest
                    -- Start request when page loads
                    , xhrToCommand <$> initialRequest
                    -- "Reverse Messages" checkbox
                    , changeOrder <$> _checkbox_change showReverse
                    -- "Only show today" checkbox
                    , changeFilter today <$>  _checkbox_change showToday
                    -- We start fetching the data
                    , const (SetStatus Loading) <$> fforMaybe zippedDyn changeInput
                    ]
            _ <- dyn (stateToWidget <$> state)
        return ()

homePageLink :: DomBuilder t m => m ()
homePageLink = do
    text "#"
    elAttr "a" ("href" =: itaLink ) (text "haskell.it")
    text " log"

itaLink :: T.Text
itaLink = "http://haskell-ita.it/"

changeOrder :: Bool -> Command
changeOrder True = SetOrder Reversed
changeOrder False = SetOrder Normal

changeFilter :: UTCTime -> Bool -> Command
changeFilter now True = SetFilter (OnlyToday now)
changeFilter _ False = SetFilter None

changeInput :: QueryDates -> Maybe (XhrRequest ())
changeInput Neither = Nothing
changeInput notNull = Just $ apiRequest finalParams
    where
        finalParams = toParams notNull

apiRequest :: T.Text -> XhrRequest ()
apiRequest params = xhrRequest "GET" (apiUrl <> params) def

apiUrl :: T.Text
apiUrl = "/api/log/"

toParams :: QueryDates -> T.Text
toParams (From from) = "?from=" <> from
toParams (To to) = "?to=" <> to
toParams (Both from to) = "?from=" <> from <> "&" <> "to=" <> to
toParams Neither = ""

stateToWidget :: (MonadWidget t m) => State -> m ()
stateToWidget localState = elClass "div" "data-container" $ case _httpStatus localState of
    Ok -> do
        elClass "p" "statusMessage" (text . showLoadedStatus $ localState)
        elClass "p" "fromDate" (text (maybe invalidDate (showDate . runFD) $ _fromDate localState))
        elClass "p" "toDate" (text (maybe invalidDate (showDate . runTD) $ _toDate localState))
        elClass "table" "logtable" $ buildTable localState
    x -> elClass "p" "statusMessage" (text . showStatus $ x)

buildTable :: (MonadWidget t m ) => State -> m ()
buildTable localState = do
    elClass "thead" "loghead" $
        el "tr" $ do
            el "th" $ text "Timestamp (UTC)"
            el "th" $ text "Nickname"
            el "th" $ text "Message"
    elClass "tbody" "logbody" $ mapM_ (toRow (_dateFilter localState)) logRows
    where logRows = case _order localState of
              Normal -> fromMaybe [] (_messages localState)
              Reversed -> reverse $ fromMaybe [] (_messages localState)

showLoadedStatus :: State -> T.Text
showLoadedStatus localState =
    showStatus (_httpStatus localState)
    <> " "
    <> (T.pack . show . length) (fromMaybe [] (_messages localState))
    <> " messages"

toRow :: (MonadWidget t m) => Filter -> PrivMsg -> m ()
toRow (OnlyToday today) msg = when (isToday today msg) (buildRow msg)
toRow _ msg = buildRow msg

buildRow :: (MonadWidget t m) => PrivMsg -> m ()
buildRow msg = elClass "tr" "logrow" $ do
    el "td" (text . showDate $ timestamp msg)
    let md5Hash :: MD5 = hash . encodeUtf8 . nickname $ msg
    let nickColor = "#" <> T.take 6 (T.pack . show $ md5Hash)
    elAttr "td" ("style" =: ("color: " <> nickColor)) (text $ nickname msg)
    el "td" (text $ message msg)

showDate :: UTCTime -> T.Text
showDate = T.pack . formatTime defaultTimeLocale "%F %T"

invalidDate :: T.Text
invalidDate = "Invalid Date"

showStatus :: Status -> T.Text
showStatus Loading = "Loading"
showStatus Lib.Invalid = "Invalid date"
showStatus ServerError = "Error while fetching data"
showStatus Ok = "Loaded"

-- Try and parse the response
xhrToCommand :: XhrResponse -> Command
xhrToCommand response = fromMaybe (SetStatus ServerError) $ UpdateMessages <$> decodeXhrResponse response

-- Update the state according to the command
updateState :: Command -> State -> State
updateState cmd oldState = case cmd of
    SetStatus newStatus -> oldState & httpStatus .~ newStatus
    UpdateMessages response -> updateStateResponse response oldState
    SetOrder newOrder -> oldState & order .~ newOrder
    SetFilter newFilter -> oldState & dateFilter .~ newFilter

updateStateResponse :: LogResponse -> State -> State
updateStateResponse (LogResponse stat fromD toD mess) oldState = case stat of
    Ok -> oldState & Types.messages .~ Just mess
                   & Types.fromDate .~ Just (FD fromD)
                   & Types.toDate   .~ Just (TD toD)
                   & Types.httpStatus .~ Ok
    x -> oldState & Types.messages .~ Nothing
                  & Types.fromDate .~ Nothing
                  & Types.toDate   .~ Nothing
                  & Types.httpStatus .~ x

-- Is the PrivMsg the same day as the utctime?
isToday :: UTCTime -> PrivMsg -> Bool
isToday today msg = diffDays (utctDay today) (utctDay . timestamp $ msg) < 1

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
    | isJust (parseTimeM False defaultTimeLocale "%F" (T.unpack date) :: Maybe UTCTime) = ValidDate
    | otherwise = InvalidDate

newCheckBox :: (DomBuilder t m, PostBuild t m) => T.Text -> m (Checkbox t)
newCheckBox label = el "div" $ do
    text label
    checkbox False def

newDateInput :: (MonadWidget t m) => T.Text -> m (TextInput t)
newDateInput defText = textInput $ def & textInputConfig_inputType .~ "date"
                                           & textInputConfig_attributes .~ constDyn attri
    where
        attri = "placeholder" =: defText
