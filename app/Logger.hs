{-# LANGUAGE OverloadedStrings #-}

module Logger
  ( putLog
  ) where

import Control.Monad.IO.Class
import Data.Time.LocalTime
import System.Log.FastLogger

putLog :: (MonadIO m, ToLogStr s) => LoggerSet -> s -> m ()
putLog logset message = liftIO $ do
  now <- getZonedTime
  pushLogStrLn logset $ toLogStr (show now) <> ": " <> toLogStr message
