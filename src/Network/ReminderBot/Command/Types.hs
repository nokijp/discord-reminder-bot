module Network.ReminderBot.Command.Types
  ( Command(..)
  , CommandTime(..)
  , CommandError(..)
  , commandTimeToLocalTime
  ) where

import Control.Monad
import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.LocalTime
import Network.ReminderBot.HashCode

data Command = CommandAdd CommandTime Text
             | CommandList
             | CommandRemove HashCode
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

commandTimeToLocalTime :: LocalTime -> CommandTime -> Maybe LocalTime
commandTimeToLocalTime now (CommandTimeHM hour minute) =
  let (year, month, day) = toGregorian $ localDay now
  in toLocalTime now year month day hour minute
commandTimeToLocalTime now (CommandTimeMDHM month day hour minute) =
  let (year, _, _) = toGregorian $ localDay now
  in toLocalTime now year month day hour minute
commandTimeToLocalTime now (CommandTimeYMDHM year month day hour minute) =
  toLocalTime now year month day hour minute

toLocalTime :: LocalTime -> Integer -> Int -> Int -> Int -> Int -> Maybe LocalTime
toLocalTime now year month day hour minute = do
  t <- LocalTime <$> fromGregorianValid year month day <*> makeTimeOfDayValid hour minute 0
  guard $ t > now
  return t
