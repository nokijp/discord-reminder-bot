module Network.ReminderBot.Schedule
  ( Schedule(..)
  ) where

import Data.Text (Text)
import Data.Word

data Schedule = Schedule { scheduleChannel :: Word64
                         , scheduleSource :: Word64
                         , scheduleMessage :: Text
                         } deriving (Show, Eq)
