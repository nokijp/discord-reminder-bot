module Network.ReminderBot.Command.Types
  ( Command(..)
  , CommandTime(..)
  , CommandError(..)
  ) where

import Data.Text (Text)
import Network.ReminderBot.Schedule

data Command = CommandAdd CommandTime Text
             | CommandListGuild
             | CommandListChannel
             | CommandRemove ScheduleID
               deriving (Show, Eq)

data CommandTime = CommandTimeHM Int Int
                 | CommandTimeMDHM Int Int Int Int
                 | CommandTimeYMDHM Integer Int Int Int Int
                   deriving (Show, Eq)

data CommandError = AddArgumentError
                  | ListArgumentError
                  | RemoveArgumentError
                  | UnknownCommandError
                    deriving (Show, Eq)
