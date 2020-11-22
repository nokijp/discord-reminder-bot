{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.ReminderBot.Schedule
  ( GuildID(..)
  , ChannelID(..)
  , MessageID(..)
  , UserID(..)
  , Schedule(..)
  , ScheduleID(..)
  , scheduleIDToHashCode
  ) where

import Data.Binary
import Data.Text (Text)
import Data.Time.Clock
import Network.ReminderBot.Internal.HashCode

newtype GuildID = GuildID Word64 deriving (Show, Eq, Ord, Enum, Num, Real, Integral)
newtype ChannelID = ChannelID Word64 deriving (Show, Eq, Ord, Enum, Num, Real, Integral)
newtype MessageID = MessageID Word64 deriving (Show, Eq, Ord, Enum, Num, Real, Integral, Binary)
newtype UserID = UserID Word64 deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

data Schedule = Schedule { scheduleTime :: UTCTime
                         , scheduleGuildID :: GuildID
                         , scheduleChannelID :: ChannelID
                         , scheduleUserID :: UserID
                         , scheduleMessageID :: MessageID
                         , scheduleMessage :: Text
                         } deriving (Show, Eq)

newtype ScheduleID = ScheduleID HashCode deriving (Show, Eq)

scheduleIDToHashCode :: ScheduleID -> HashCode
scheduleIDToHashCode (ScheduleID code) = code
