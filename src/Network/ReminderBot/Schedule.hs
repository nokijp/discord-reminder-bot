{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.ReminderBot.Schedule
  ( GuildID(..)
  , ChannelID(..)
  , MessageID(..)
  , UserID(..)
  , Schedule(..)
  ) where

import Data.Binary
import Data.Text (Text)
import Network.ReminderBot.HashCode

newtype GuildID = GuildID Word64 deriving (Show, Eq, Ord, Enum, Num, Real, Integral)
newtype ChannelID = ChannelID Word64 deriving (Show, Eq, Ord, Enum, Num, Real, Integral)
newtype MessageID = MessageID Word64 deriving (Show, Eq, Ord, Enum, Num, Real, Integral, Binary)
newtype UserID = UserID Word64 deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

data Schedule = Schedule { scheduleGuild :: GuildID
                         , scheduleChannel :: ChannelID
                         , scheduleIdentifier :: HashCode
                         , scheduleUser :: UserID
                         , scheduleMessage :: Text
                         } deriving (Show, Eq)
