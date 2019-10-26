module Network.ReminderBot.Schedule
  ( ChannelID
  , MessageID
  , Schedule(..)
  ) where

import Data.Text (Text)
import Data.Word
import Network.ReminderBot.HashCode

type ChannelID = Word64
type MessageID = Word64

data Schedule = Schedule { scheduleChannel :: ChannelID
                         , scheduleIdentifier :: HashCode
                         , scheduleMessage :: Text
                         } deriving (Show, Eq)
