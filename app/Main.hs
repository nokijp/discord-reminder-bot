{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import CommandReceiver
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Discord
import Logger
import Network.ReminderBot.ScheduleStore
import System.Environment
import System.Log.FastLogger

main :: IO ()
main = do
  (token, logger) <- parseArguments <$> getArgs

  bracket logger flushLogStr $ \logset ->
    retry logset $ \reset -> do
      err <- runDiscord $ def { discordToken = token
                              , discordOnStart = const reset
                              , discordOnEvent = receiveCommand logset defaultScheduleStoreConfig
                              }
      putLog logset $ "error: " <> err

parseArguments :: [String] -> (Text, IO LoggerSet)
parseArguments [tokenString, logFilePath] = (T.pack tokenString, newFileLoggerSet defaultBufSize logFilePath)
parseArguments [tokenString] = (T.pack tokenString, newStderrLoggerSet defaultBufSize)
parseArguments _ = fail "invalid arguments"

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
