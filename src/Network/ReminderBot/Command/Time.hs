{-# LANGUAGE OverloadedStrings #-}

module Network.ReminderBot.Command.Time
  ( commandTimeToLocalTime
  ) where

import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.LocalTime
import Network.ReminderBot.Command.Types

commandTimeToLocalTime :: LocalTime -> CommandTime -> Either Text LocalTime
commandTimeToLocalTime now (CommandTimeHM hour minute) = d1 <|> d2
  where
    d1 = toFutureLocalTimeDHM now today hour minute
    d2 = toFutureLocalTimeDHM now (succ today) hour minute
    today = localDay now
commandTimeToLocalTime now (CommandTimeMDHM month day hour minute) = d1 <|> d2
  where
    d1 = toFutureLocalTime now year month day hour minute
    d2 = toFutureLocalTime now (succ year) month day hour minute
    (year, _, _) = toGregorian $ localDay now
commandTimeToLocalTime now (CommandTimeYMDHM year month day hour minute) =
  toFutureLocalTime now year month day hour minute

toFutureLocalTime :: LocalTime -> Integer -> Int -> Int -> Int -> Int -> Either Text LocalTime
toFutureLocalTime now year month day hour minute = do
  date <- toEither "invalid date" $ fromGregorianValid year month day
  toFutureLocalTimeDHM now date hour minute

toFutureLocalTimeDHM :: LocalTime -> Day -> Int -> Int -> Either Text LocalTime
toFutureLocalTimeDHM now date hour minute = do
  t <- if (hour, minute) == (24, 0)
       then return $ LocalTime (succ date) midnight
       else LocalTime date <$> toEither "invalid date" (makeTimeOfDayValid hour minute 0)
  if t > now then Right t else Left "past date"

toEither :: a -> Maybe b -> Either a b
toEither a = maybe (Left a) Right

(<|>) :: Either a b -> Either a b -> Either a b
a <|> b = either (const b) Right a
