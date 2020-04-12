{-# LANGUAGE OverloadedStrings #-}

module CommandReceiver
  ( receiveCommand
  ) where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.Char
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format
import Data.Time.LocalTime
import Discord
import Discord.Types
import Discord.Requests
import Exts
import Logger
import Network.ReminderBot.Command.Parser
import Network.ReminderBot.ScheduleStore
import System.Log.FastLogger

receiveCommand :: LoggerSet -> ScheduleStoreConfig -> DiscordHandle -> Event -> IO ()
receiveCommand logset config dis (MessageCreate m) | isNotFromBot m = fmap (fromMaybe ()) $ runMaybeT $ do
  botUserID <- lift $ spyBotUserID dis
  command <- maybeT $ parseMessage botUserID $ messageText m
  guildID <- maybeT $ fromIntegral <$> messageGuild m
  let
    channelID = fromIntegral $ messageChannel m
    messageID = fromIntegral $ messageId m
    userID = fromIntegral $ userId $ messageAuthor m
  now <- liftIO getZonedTime
  responseEither <- lift $ runExceptT $ runCommand logset config now guildID channelID messageID userID command
  let response = either ("error: " <>) id responseEither
  status <- lift $ restCall dis $ CreateMessage (messageChannel m) response
  either (putLog logset . show) (const $ return ()) status
receiveCommand _ _ _ _ = return ()

parseMessage :: UserId -> Text -> Maybe (Either CommandError Command)
parseMessage botUserID message = if prefix `T.isPrefixOf` message
                                 then Just $ parseCommand commandText
                                 else Nothing
  where
    prefix = "<@" <> toText (toInteger botUserID) <> ">"
    commandText = T.drop (T.length prefix) message

runCommand :: (MonadIO m, MonadCatch m)
           => LoggerSet
           -> ScheduleStoreConfig
           -> ZonedTime
           -> GuildID
           -> ChannelID
           -> MessageID
           -> UserID
           -> Either CommandError Command
           -> ExceptT Text m Text
runCommand logset config now guildID channelID messageID userID (Right (CommandAdd commandTime message)) = do
  localTime <- maybeE "invalid date" $ commandTimeToLocalTime (zonedTimeToLocalTime now) commandTime
  let
    zone = zonedTimeZone now
    time = localTimeToUTC zone localTime
    schedule = Schedule { scheduleTime = time
                        , scheduleGuildID = guildID
                        , scheduleChannelID = channelID
                        , scheduleUserID = userID
                        , scheduleMessageID = messageID
                        , scheduleMessage = message
                        }
  scheduleID <- scheduleE logset $ addSchedule config schedule
  return $ "added: " <> displaySchedule zone (scheduleID, schedule)
runCommand logset config now guildID _ _ _ (Right CommandListGuild) =
  scheduleE logset $ displaySchedules (zonedTimeZone now) <$> listGuildSchedule config guildID
runCommand logset config now _ channelID _ _ (Right CommandListChannel) =
  scheduleE logset $ displaySchedules (zonedTimeZone now) <$> listChannelSchedule config channelID
runCommand logset config _ _ _ _ _ (Right (CommandRemove scheduleID)) = do
  isSucceeded <- scheduleE logset $ removeSchedule config scheduleID
  if isSucceeded
  then return $ "removed: " <> toText scheduleID
  else throwE $ "not found: " <> toText scheduleID
runCommand _ _ _ _ _ _ _ (Left errorType) = errorMessage errorType
  where
    errorMessage :: Monad m => CommandError -> ExceptT Text m Text
    errorMessage AddArgumentError    = throwE addUsage
    errorMessage ListArgumentError   = throwE listUsage
    errorMessage RemoveArgumentError = throwE removeUsage
    errorMessage UnknownCommandError = return $ T.unlines [addUsage, listUsage, removeUsage]
    addUsage = "add [[Y/]M/D] h:m {MESSAGE}"
    listUsage = "ls [all]"
    removeUsage = "rm {ID}"

displaySchedules :: TimeZone -> [(ScheduleID, Schedule)] -> Text
displaySchedules _ [] = "no reminders set"
displaySchedules zone schedules = T.unlines $ displaySchedule zone <$> schedules

displaySchedule :: TimeZone -> (ScheduleID, Schedule) -> Text
displaySchedule zone (scheduleID, schedule) = T.unwords [idText, timeText, channelRef, messagePreview]
  where
    idText = "`ID:" <> toText scheduleID <> "`"
    timeText = T.pack $ formatTime defaultTimeLocale "🗓 %Y/%m/%d %T" $ utcToZonedTime zone $ scheduleTime schedule
    channelRef = "<#" <> toText (toInteger $ scheduleChannelID schedule) <> ">"
    messagePreview = formatMessage 30 $ scheduleMessage schedule

formatMessage :: Int -> Text -> Text
formatMessage len = truncateText . spaceToSpace . removeSpecialCharacters . T.strip
  where
    truncateText t = if T.length t <= len then t else T.take len t <> "..."
    spaceToSpace = T.map (\c -> if isSpace c then ' ' else c)
    removeSpecialCharacters = T.filter (not . (`elem` ['_', '~', '*', '`']))


scheduleE :: (MonadIO m, MonadCatch m) => LoggerSet -> m a -> ExceptT Text m a
scheduleE logset m = do
  res <- lift $ trySchedule m
  either (\e -> putLog logset e >> throwE "internal error") return res


spyBotUserID :: DiscordHandle -> IO UserId
spyBotUserID dis = (\(Cache user _ _ _) -> userId user) <$> readCache dis

isNotFromBot :: Message -> Bool
isNotFromBot = not . userIsBot . messageAuthor
