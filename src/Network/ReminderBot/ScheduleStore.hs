{-# LANGUAGE OverloadedStrings #-}

module Network.ReminderBot.ScheduleStore
  ( module Network.ReminderBot.Schedule
  , ScheduleStoreConfig(..)
  , defaultScheduleStoreConfig
  , addSchedule
  , getFirstSchedule
  , getScheduleBefore
  , removeScheduleBefore
  , listGuildSchedule
  , listChannelSchedule
  , removeSchedule
  , trySchedule
  , isUp
  ) where

import Control.Arrow
import Control.Monad.Catch
import Control.Monad.Trans
import Data.Int
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Word
import Database.MongoDB hiding (host)
import Network.ReminderBot.Internal.HashCode
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
            -> Schedule
            -> m ScheduleID
addSchedule config schedule = runAction config $ do
  let document = [ scheduleTimeLabel =: UTC (scheduleTime schedule)
                 , guildLabel =: (Int64 $ fromIntegral $ scheduleGuildID schedule)
                 , channelLabel =: (Int64 $ fromIntegral $ scheduleChannelID schedule)
                 , sourceLabel =: (Int64 $ fromIntegral $ scheduleMessageID schedule)
                 , userLabel =: (Int64 $ fromIntegral $ scheduleUserID schedule)
                 , messageLabel =: String (scheduleMessage schedule)
                 ]
  insert_ (collectionName config) document

  let identifier = ScheduleID $ hashCode $ fromIntegral $ scheduleMessageID schedule
  return identifier

getFirstSchedule :: MonadIO m => ScheduleStoreConfig -> m (Maybe (ScheduleID, Schedule))
getFirstSchedule config = runAction config $ do
  let query = (select [] $ collectionName config) { sort = [scheduleTimeLabel := Int32 1] }
  documentMaybe <- findOne query
  return $ extractSchedule =<< documentMaybe

getScheduleBefore :: MonadIO m => ScheduleStoreConfig -> UTCTime -> m [(ScheduleID, Schedule)]
getScheduleBefore config time = runAction config $ do
  cursor <- find $ selectBefore config time
  documents <- allDocuments cursor
  return $ mapMaybe extractSchedule documents

removeScheduleBefore :: MonadIO m => ScheduleStoreConfig -> UTCTime -> m ()
removeScheduleBefore config time = runAction config $
  delete $ selectBefore config time

listGuildSchedule :: MonadIO m => ScheduleStoreConfig -> GuildID -> m [(ScheduleID, Schedule)]
listGuildSchedule config guildID = listSchedule config [guildLabel =: Int64 (fromIntegral guildID)]

listChannelSchedule :: MonadIO m => ScheduleStoreConfig -> ChannelID -> m [(ScheduleID, Schedule)]
listChannelSchedule config channelID = listSchedule config [channelLabel =: Int64 (fromIntegral channelID)]

listSchedule :: MonadIO m => ScheduleStoreConfig -> Selector -> m [(ScheduleID, Schedule)]
listSchedule config sel = runAction config $ do
  let query = (select sel $ collectionName config) { sort = [scheduleTimeLabel := Int32 1] }
  cursor <- find query
  documents <- allDocuments cursor
  return $ mapMaybe extractSchedule documents

removeSchedule :: MonadIO m => ScheduleStoreConfig -> ScheduleID -> m Bool
removeSchedule config identifier = runAction config $ do
  let
    source = hashCodeInv $ scheduleIDToHashCode identifier
    sel = [sourceLabel =: Int64 (fromIntegral source)]
    query :: Select a => a
    query = select sel $ collectionName config
  documentMaybe <- findOne query
  case documentMaybe of
    Just _ -> True <$ delete query
    Nothing -> return False


extractSchedule :: Document -> Maybe (ScheduleID, Schedule)
extractSchedule d = do
  time <- d !? scheduleTimeLabel
  guild <- d !? guildLabel
  channel <- d !? channelLabel
  source <- d !? sourceLabel
  user <- d !? userLabel
  message <- d !? messageLabel
  let
    schedule = Schedule { scheduleTime = time
                        , scheduleGuildID = fromIntegral (guild :: Int64)
                        , scheduleChannelID = fromIntegral (channel :: Int64)
                        , scheduleUserID = fromIntegral (user :: Int64)
                        , scheduleMessageID = fromIntegral (source :: Int64)
                        , scheduleMessage = message
                        }
    identifier = ScheduleID $ hashCode $ fromIntegral (source :: Int64)
  return (identifier, schedule)

selectBefore :: Select a => ScheduleStoreConfig -> UTCTime -> a
selectBefore config time = select [scheduleTimeLabel =: ["$lte" =: UTC time]] $ collectionName config

allDocuments :: MonadIO m => Cursor -> Action m [Document]
allDocuments cursor = do
  part <- nextBatch cursor
  if null part
  then return []
  else (part ++) <$> allDocuments cursor

ensureIndices :: MonadIO m => ScheduleStoreConfig -> Action m ()
ensureIndices config = do
  ensureIndex $ (index (collectionName config) [scheduleTimeLabel =: Int32 1]) { iExpireAfterSeconds = Just 3600 }
  ensureIndex $ index (collectionName config) [channelLabel =: Int32 1]
  ensureIndex $ index (collectionName config) [guildLabel =: Int32 1]

runAction :: MonadIO m => ScheduleStoreConfig -> Action IO a -> m a
runAction config action = liftIO $ bracket (connect host) close $ \pipe -> access pipe master (dbName config) action'
  where
    host = Host (mongoHost config) (PortNumber $ fromIntegral $ mongoPort config)
    action' = ensureIndices config >> action


trySchedule :: (MonadIO m, MonadCatch m) => m a -> m (Either Text a)
trySchedule m = left (\e -> T.pack $ displayException (e :: SomeException)) <$> try m

isUp :: (MonadIO m, MonadCatch m) => ScheduleStoreConfig -> m Bool
isUp config = either (const False :: SomeException -> Bool) (const True) <$> try (runAction config $ return ())


scheduleTimeLabel :: Label
scheduleTimeLabel = "scheduleTime"

guildLabel :: Label
guildLabel = "guild"

channelLabel :: Label
channelLabel = "channel"

sourceLabel :: Label
sourceLabel = "source"

userLabel :: Label
userLabel = "user"

messageLabel :: Label
messageLabel = "message"
