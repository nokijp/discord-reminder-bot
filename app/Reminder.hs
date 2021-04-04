{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Reminder
  ( forkRemindLoop
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.IORef
import Data.Time.Clock
import qualified Data.Text as T
import Discord
import Discord.Requests
import Exts
import Logger
import Network.ReminderBot.ScheduleStore
import RequestExts
import System.Log.FastLogger

forkRemindLoop :: IORef (Maybe DiscordHandle) -> LoggerSet -> ScheduleStoreConfig -> IO ThreadId
forkRemindLoop handleRef logset config = forkIO $ runEveryMinute handleRef logset $ remind logset config

remind :: LoggerSet -> ScheduleStoreConfig -> DiscordHandler ()
remind logset config = void $ runMaybeT $ do
  now <- liftIO getCurrentTime
  schedules <- runScheduleM logset $ getScheduleBeforeOrEqual config now
  forM_ schedules $ \(scheduleID, schedule) -> do
    postReminder logset schedule
    runScheduleM logset $ removeSchedule config scheduleID

runEveryMinute :: IORef (Maybe DiscordHandle) -> LoggerSet -> DiscordHandler () -> IO ()
runEveryMinute handleRef logset action = forever $ do
  maybeHandle <- readIORef handleRef
  maybe ($putLog' logset ("skip" :: String)) (runReaderT action) maybeHandle

  now <- getCurrentTime
  let
    nowZeroSec = fromIntegral $ ((floor $ utctDayTime now :: Integer) `div` 60) * 60
    next = addUTCTime 60 $ UTCTime (utctDay now) nowZeroSec
    delay = diffUTCTime next now
    delayInMicros = if delay > 0 then floor (delay * 1000 * 1000) else 0
  threadDelay delayInMicros

postReminder :: LoggerSet -> Schedule -> MaybeT DiscordHandler ()
postReminder logset schedule = do
  let
    channelID = fromIntegral $ scheduleChannelID schedule
    messageReferenceID = fromIntegral $ scheduleMessageID schedule
  messageReference <- lift $ restCall $ GetChannelMessage (channelID, messageReferenceID)
  messageExists <- case messageReference of
                     Right _ -> return True
                     Left (RestCallErrorCode 404 _ _) -> return False
                     Left e -> $putLog' logset (show e) >> exitM
  let
    rawMessage = scheduleMessage schedule
    refMessage = userRef <> connector <> rawMessage
    userRef = "<@" <> toText (scheduleUserID schedule) <> ">"
    connector = if "\n" `T.isInfixOf` rawMessage then "\n" else " "
  status <- lift $ if messageExists
                   then restCall $ CreateReply channelID messageReferenceID rawMessage
                   else restCall $ CreateMessage channelID refMessage
  either (\e -> $putLog' logset (show e) >> exitM) (const $ return ()) status

runScheduleM :: (MonadIO m, MonadCatch m) => LoggerSet -> m a -> MaybeT m a
runScheduleM logset action = do
  res <- lift $ trySchedule action
  either (\e -> $putLog' logset e >> exitM) return res
