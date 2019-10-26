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
import Network.ReminderBot.HashCode
import Network.ReminderBot.Schedule

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

addSchedule :: MonadIO m
            => ScheduleStoreConfig
            -> UTCTime
            -> ChannelID
            -> MessageID
            -> Text
            -> m Schedule
addSchedule config time channelID messageID message = runAction config $ do
  ensureIndices config

  let
    identifier = hashCode messageID
    document = [ scheduleTimeLabel =: UTC time
               , channelLabel =: (Int64 $ fromIntegral channelID)
               , sourceLabel =: (Int64 $ fromIntegral messageID)
               , identifierLabel =: (Int32 $ fromIntegral identifier)
               , messageLabel =: String (scheduleMessage schedule)
               ]
    schedule = Schedule { scheduleChannel = channelID
                        , scheduleIdentifier = identifier
                        , scheduleMessage = message
                        }
  insert_ (collectionName config) document

  return schedule

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

listSchedule :: MonadIO m => ScheduleStoreConfig -> ChannelID -> m [(UTCTime, Schedule)]
listSchedule config channelID = runAction config $ do
  ensureIndices config

  let
    sel = [channelLabel =: Int64 (fromIntegral channelID)]
    order = [scheduleTimeLabel := Int32 1]
    query = (select sel $ collectionName config) { sort = order }
  cursor <- find query
  documents <- nextBatch cursor

  return $ mapMaybe extractSchedule documents

removeSchedule :: MonadIO m => ScheduleStoreConfig -> ChannelID -> HashCode -> m Bool
removeSchedule config channelID identifier = runAction config $ do
  let
    sel = [ channelLabel =: (Int64 $ fromIntegral channelID)
          , identifierLabel =: Int32 (fromIntegral identifier)
          ]
    query :: Select a => a
    query = select sel $ collectionName config
  documentMaybe <- findOne query
  case documentMaybe of
    Just _ -> True <$ delete query
    Nothing -> return False


extractSchedule :: Document -> Maybe (UTCTime, Schedule)
extractSchedule d = do
  time <- d !? scheduleTimeLabel
  channel <- d !? channelLabel
  identifier <- d !? identifierLabel
  message <- d !? messageLabel
  let schedule = Schedule { scheduleChannel = fromIntegral (channel :: Int64)
                          , scheduleIdentifier = fromIntegral (identifier :: Int32)
                          , scheduleMessage = message
                          }
  return (time, schedule)


ensureIndices :: MonadIO m => ScheduleStoreConfig -> Action m ()
ensureIndices config = do
  ensureIndex $ (index (collectionName config) [scheduleTimeLabel =: Int32 1]) { iExpireAfterSeconds = Just 3600 }
  ensureIndex $ index (collectionName config) [channelLabel =: Int32 1]
  ensureIndex $ index (collectionName config) [identifierLabel =: Int32 1]

runAction :: MonadIO m => ScheduleStoreConfig -> Action IO a -> m a
runAction config f = liftIO $ bracket (connect host) close $ \pipe -> access pipe master (dbName config) f where
  host = Host (mongoHost config) (PortNumber $ fromIntegral $ mongoPort config)


scheduleTimeLabel :: Label
scheduleTimeLabel = "scheduleTime"

channelLabel :: Label
channelLabel = "channel"

sourceLabel :: Label
sourceLabel = "source"

identifierLabel :: Label
identifierLabel = "identifier"

messageLabel :: Label
messageLabel = "message"
