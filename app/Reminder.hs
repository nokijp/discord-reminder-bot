{-# LANGUAGE OverloadedStrings #-}

module Reminder
  ( forkRemindLoop
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.Time.Clock
import qualified Data.Text as T
import Discord
import Discord.Requests
import Logger
import Network.ReminderBot.ScheduleStore
import System.Log.FastLogger

forkRemindLoop :: LoggerSet -> ScheduleStoreConfig -> DiscordHandle -> IO ThreadId
forkRemindLoop logset config dis = forkIO $ runEveryMinute $ remind logset config dis

remind :: LoggerSet -> ScheduleStoreConfig -> DiscordHandle -> UTCTime -> IO ()
remind logset config dis now = void $ runMaybeT $ do
  schedules <- runScheduleM logset $ getScheduleBefore config now
  mapM_ (postReminder logset dis) schedules
  _ <- runScheduleM logset $ removeScheduleBefore config now
  return ()

runEveryMinute :: (UTCTime -> IO ()) -> IO ()
runEveryMinute action = forever $ do
  now <- getCurrentTime
  _ <- forkIO $ action now
  let
    nowZeroSec = fromIntegral $ ((floor $ utctDayTime now :: Integer) `div` 60) * 60
    next = addUTCTime 60 $ UTCTime (utctDay now) nowZeroSec
    delay = diffUTCTime next now
    delayInMicros = floor (delay * 1000 * 1000)
  threadDelay delayInMicros

postReminder :: MonadIO m => LoggerSet -> DiscordHandle -> Schedule -> MaybeT m ()
postReminder logset dis schedule = do
  let
    rawMessage = scheduleMessage schedule
    userRef = "<@" <> (T.pack $ show $ toInteger $ scheduleUser schedule) <> ">"
    connector = if "\n" `T.isInfixOf` rawMessage then "\n" else " "
    message = userRef <> connector <> rawMessage
  status <- liftIO $ restCall dis $ CreateMessage (fromIntegral $ scheduleChannel schedule) message
  either (\e -> putLog logset (show e) >> exitM) (const $ return ()) status

runScheduleM :: (MonadIO m, MonadCatch m) => LoggerSet -> m a -> MaybeT m a
runScheduleM logset action = do
  res <- lift $ trySchedule action
  either (\e -> putLog logset e >> exitM) return res

exitM :: Monad m => MaybeT m a
exitM = MaybeT $ return Nothing
