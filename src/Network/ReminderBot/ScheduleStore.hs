{-# LANGUAGE OverloadedStrings #-}

module Network.ReminderBot.ScheduleStore
  ( ScheduleStoreConfig(..)
  , defaultScheduleStoreConfig
  , Schedule(..)
  , addSchedule
  , getSchedule
  , listSchedule
  , removeSchedule
  ) where

import Control.Monad.Catch
import Control.Monad.Trans
import Data.Int
import Data.Maybe
import Data.Text (Text)
import Data.Time.Clock
import Data.Word
import Database.MongoDB hiding (host)

data ScheduleStoreConfig = ScheduleStoreConfig { mongoHost :: String
                                               , mongoPort :: Word16
                                               , dbName :: Text
                                               , collectionName :: Text
                                               }

defaultScheduleStoreConfig :: ScheduleStoreConfig
defaultScheduleStoreConfig = ScheduleStoreConfig { mongoHost = "localhost"
                                                 , mongoPort = 27017
                                                 , dbName = "reminderBot"
                                                 , collectionName = "scheduleStore"
                                                 }

data Schedule = Schedule { scheduleChannel :: Word64
                         , scheduleSource :: Word64
                         , scheduleMessage :: Text
                         } deriving (Show, Eq)

addSchedule :: MonadIO m => ScheduleStoreConfig -> UTCTime -> Schedule -> m ()
addSchedule config time schedule = runAction config $ do
  ensureIndices config

  let document = [ scheduleTimeLabel =: UTC time
                 , channelLabel =: (Int64 $ fromIntegral $ scheduleChannel schedule)
                 , sourceLabel =: (Int64 $ fromIntegral $ scheduleSource schedule)
                 , messageLabel =: String (scheduleMessage schedule)
                 ]
  insert_ (collectionName config) document

getSchedule :: MonadIO m => ScheduleStoreConfig -> UTCTime -> m [Schedule]
getSchedule config time = runAction config $ do
  ensureIndices config

  let
    query :: Select a => a
    query = select [scheduleTimeLabel =: UTC time] $ collectionName config
  cursor <- find query
  documents <- nextBatch cursor
  delete query

  return $ snd <$> mapMaybe extractSchedule documents

listSchedule :: MonadIO m => ScheduleStoreConfig -> Word64 -> m [(UTCTime, Schedule)]
listSchedule config channel = runAction config $ do
  ensureIndices config

  let
    sel = [channelLabel =: Int64 (fromIntegral channel)]
    order = [scheduleTimeLabel := Int32 1]
    query = (select sel $ collectionName config) { sort = order }
  cursor <- find query
  documents <- nextBatch cursor

  return $ mapMaybe extractSchedule documents

removeSchedule :: MonadIO m => ScheduleStoreConfig -> Word64 -> m Bool
removeSchedule config source = runAction config $ do
  let
    query :: Select a => a
    query = select [sourceLabel =: Int64 (fromIntegral source)] $ collectionName config
  documentMaybe <- findOne query
  case documentMaybe of
    Just _ -> True <$ delete query
    Nothing -> return False


extractSchedule :: Document -> Maybe (UTCTime, Schedule)
extractSchedule d = do
  time <- d !? scheduleTimeLabel
  channel <- d !? channelLabel
  source <- d !? sourceLabel
  message <- d !? messageLabel
  let schedule = Schedule { scheduleChannel = fromIntegral (channel :: Int64)
                          , scheduleSource = fromIntegral (source :: Int64)
                          , scheduleMessage = message
                          }
  return (time, schedule)


ensureIndices :: MonadIO m => ScheduleStoreConfig -> Action m ()
ensureIndices config = do
  ensureIndex $ (index (collectionName config) [scheduleTimeLabel =: Int32 1]) { iExpireAfterSeconds = Just 3600 }
  ensureIndex $ index (collectionName config) [channelLabel =: Int32 1]
  ensureIndex $ index (collectionName config) [sourceLabel =: Int32 1]

runAction :: MonadIO m => ScheduleStoreConfig -> Action IO a -> m a
runAction config f = liftIO $ bracket (connect host) close $ \pipe -> access pipe master (dbName config) f where
  host = Host (mongoHost config) (PortNumber $ fromIntegral $ mongoPort config)


scheduleTimeLabel :: Label
scheduleTimeLabel = "scheduleTime"

channelLabel :: Label
channelLabel = "channel"

sourceLabel :: Label
sourceLabel = "source"

messageLabel :: Label
messageLabel = "message"
