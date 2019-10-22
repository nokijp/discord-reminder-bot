module Network.ReminderBot.Command.Types
  ( Command(..)
  , CommandTime(..)
  ) where

import Data.Text (Text)
import Data.Word

data Command = CommandAdd CommandTime Text
             | CommandList
             | CommandRemove Word64
             | CommandHelp
               deriving (Show, Eq)

data CommandTime = CommandTimeHM Int Int
                 | CommandTimeMDHM Int Int Int Int
                 | CommandTimeYMDHM Integer Int Int Int Int
                   deriving (Show, Eq)
