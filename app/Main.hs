{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import CommandReceiver
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Discord
import Logger
import Network.ReminderBot.ScheduleStore
import Reminder
import System.Environment
import System.Log.FastLogger

main :: IO ()
main = do
  token <- getEnvOrDefault (error "token is required") T.pack "REMINDER_BOT_DISCORD_TOKEN"
  logger <- getEnvOrDefault (newStderrLoggerSet defaultBufSize) (newFileLoggerSet defaultBufSize) "REMINDER_BOT_LOG_FILE"
  port <- getEnvOrDefault Nothing (Just . read) "REMINDER_BOT_DB_PORT"
  host <- getEnvOrDefault Nothing Just "REMINDER_BOT_DB_HOST"

  let storeConfig = defaultScheduleStoreConfig { mongoHost = fromMaybe (mongoHost defaultScheduleStoreConfig) host
                                               , mongoPort = fromMaybe (mongoPort defaultScheduleStoreConfig) port
                                               }

  bracket logger flushLogStr $ \logset ->
    retry logset $ \reset -> do
      err <- runDiscord $ def { discordToken = token
                              , discordOnStart = \dis -> reset >> void (forkRemindLoop logset storeConfig dis)
                              , discordOnEvent = receiveCommand logset storeConfig
                              }
      putLog logset $ "error: " <> err

retry :: LoggerSet -> (IO () -> IO ()) -> IO ()
retry logset f = do
  countRef <- newIORef 0
  let reset = atomicWriteIORef countRef 0
  forever $ do
    putLog logset ("start" :: Text)
    f reset
    count <- readIORef countRef
    let delayInSeconds = retryIntervalInSeconds count
    putLog logset $ "retry (wait = " <> show delayInSeconds <> " s)"
    threadDelay $ delayInSeconds * 1000000
    writeIORef countRef (count + 1)

retryIntervalInSeconds :: Int -> Int
retryIntervalInSeconds n = if interval > maxInterval then maxInterval else interval
  where
    interval = 2 ^ n
    maxInterval = 1000

getEnvOrDefault :: a -> (String -> a) -> String -> IO a
getEnvOrDefault a f key = do
  s <- fromMaybe "" <$> lookupEnv key
  return $ if null s then a else f s
