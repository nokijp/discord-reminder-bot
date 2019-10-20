module TestConfig
  ( scheduleStoreHost
  , scheduleStorePort
  , scheduleStoreConfig
  ) where

import Data.Text (Text)
import Data.Word
import Network.ReminderBot.ScheduleStore

scheduleStoreHost :: String
scheduleStoreHost = "localhost"

scheduleStorePort :: Word16
scheduleStorePort = 27020

scheduleStoreConfig :: Text -> ScheduleStoreConfig
scheduleStoreConfig name = defaultScheduleStoreConfig { mongoHost = scheduleStoreHost
                                                      , mongoPort = scheduleStorePort
                                                      , dbName = name
                                                      }
