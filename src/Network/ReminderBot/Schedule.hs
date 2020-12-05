{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

module Network.ReminderBot.Schedule
  ( GuildID(..)
  , ChannelID(..)
  , MessageID(..)
  , UserID(..)
  , Schedule(..)
  , ScheduleID(..)
  ) where

import Data.Binary
import Data.Text (Text)
import Data.Time.Clock
import Network.ReminderBot.Internal.HashCode

newtype GuildID = GuildID Word64 deriving (Eq, Ord, Enum, Num, Real, Integral)
                                 deriving Show via Word64
newtype ChannelID = ChannelID Word64 deriving (Eq, Ord, Enum, Num, Real, Integral)
                                     deriving Show via Word64
newtype MessageID = MessageID Word64 deriving (Eq, Ord, Enum, Num, Real, Integral, Binary)
                                     deriving Show via Word64
newtype UserID = UserID Word64 deriving (Eq, Ord, Enum, Num, Real, Integral)
                               deriving Show via Word64

data Schedule = Schedule { scheduleTime :: UTCTime
                         , scheduleGuildID :: GuildID
                         , scheduleChannelID :: ChannelID
                         , scheduleUserID :: UserID
                         , scheduleMessageID :: MessageID
                         , scheduleMessage :: Text
                         } deriving (Show, Eq)

newtype ScheduleID = ScheduleID HashCode deriving Eq
                                         deriving Show via HashCode
