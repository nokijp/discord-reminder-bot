{-# LANGUAGE OverloadedStrings #-}

module Network.ReminderBot.Command.TimeSpec
  ( main
  , spec
  ) where

import Data.Fixed
import Data.Time.Calendar
import Data.Time.LocalTime
import Network.ReminderBot.Command.Time
import Network.ReminderBot.Command.Types
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "commandTimeToLocalTime" $ do
    context "when given CommandTimeHM" $ do
      it "modifies hours and minutes, and set seconds to 0" $
        commandTimeToLocalTime (localTime 2000 1 2 3 4 5) (CommandTimeHM 6 7) `shouldBe` Right (localTime 2000 1 2 6 7 0)
      it "returns the nearest next date" $ do
        commandTimeToLocalTime (localTime 2000 1 2 3 4 0) (CommandTimeHM 3 3) `shouldBe` Right (localTime 2000 1 3 3 3 0)
        commandTimeToLocalTime (localTime 2000 1 2 3 4 0) (CommandTimeHM 3 4) `shouldBe` Right (localTime 2000 1 3 3 4 0)
        commandTimeToLocalTime (localTime 2000 1 2 3 4 0) (CommandTimeHM 3 5) `shouldBe` Right (localTime 2000 1 2 3 5 0)
      it "can convert 24:00" $ do
        commandTimeToLocalTime (localTime 2000 1 1 23 59 59) (CommandTimeHM 24 0) `shouldBe` Right (localTime 2000 1 2 0 0 0)
        commandTimeToLocalTime (localTime 2000 1 2 0 0 0) (CommandTimeHM 24 0) `shouldBe` Right (localTime 2000 1 3 0 0 0)
        commandTimeToLocalTime (localTime 2000 1 2 0 0 1) (CommandTimeHM 24 0) `shouldBe` Right (localTime 2000 1 3 0 0 0)
      it "returns an error if the given command time has invalid values" $ do
        commandTimeToLocalTime (localTime 2000 1 1 0 0 0) (CommandTimeHM 0 60) `shouldBe` Left "invalid date"
        commandTimeToLocalTime (localTime 2000 1 1 0 0 0) (CommandTimeHM 24 1) `shouldBe` Left "invalid date"
        commandTimeToLocalTime (localTime 2000 1 1 0 0 0) (CommandTimeHM 25 0) `shouldBe` Left "invalid date"

    context "when given CommandTimeMDHM" $ do
      it "modifies month, day, hours and minutes, and set seconds to 0" $
        commandTimeToLocalTime (localTime 2000 1 2 3 4 5) (CommandTimeMDHM 6 7 8 9) `shouldBe` Right (localTime 2000 6 7 8 9 0)
      it "returns the nearest next date" $ do
        commandTimeToLocalTime (localTime 2000 1 2 3 4 0) (CommandTimeMDHM 1 2 3 3) `shouldBe` Right (localTime 2001 1 2 3 3 0)
        commandTimeToLocalTime (localTime 2000 1 2 3 4 0) (CommandTimeMDHM 1 2 3 4) `shouldBe` Right (localTime 2001 1 2 3 4 0)
        commandTimeToLocalTime (localTime 2000 1 2 3 4 0) (CommandTimeMDHM 1 2 3 5) `shouldBe` Right (localTime 2000 1 2 3 5 0)
      it "can convert 24:00" $ do
        commandTimeToLocalTime (localTime 2000 1 1 23 59 59) (CommandTimeMDHM 1 1 24 0) `shouldBe` Right (localTime 2000 1 2 0 0 0)
        commandTimeToLocalTime (localTime 2000 1 2 0 0 0) (CommandTimeMDHM 1 1 24 0) `shouldBe` Right (localTime 2001 1 2 0 0 0)
        commandTimeToLocalTime (localTime 2000 1 2 0 0 1) (CommandTimeMDHM 1 1 24 0) `shouldBe` Right (localTime 2001 1 2 0 0 0)
      it "returns an error if the given command time has invalid values" $ do
        commandTimeToLocalTime (localTime 2000 1 1 0 0 0) (CommandTimeMDHM 1 1 0 60) `shouldBe` Left "invalid date"
        commandTimeToLocalTime (localTime 2000 1 1 0 0 0) (CommandTimeMDHM 1 1 24 1) `shouldBe` Left "invalid date"
        commandTimeToLocalTime (localTime 2000 1 1 0 0 0) (CommandTimeMDHM 1 1 25 0) `shouldBe` Left "invalid date"
        commandTimeToLocalTime (localTime 2000 1 1 0 0 0) (CommandTimeMDHM 1 32 0 0) `shouldBe` Left "invalid date"
        commandTimeToLocalTime (localTime 2000 1 1 0 0 0) (CommandTimeMDHM 13 1 0 0) `shouldBe` Left "invalid date"

    context "when given CommandTimeYMDHM" $ do
      it "modifies year, month, day, hours and minutes, and set seconds to 0" $
        commandTimeToLocalTime (localTime 2000 1 2 3 4 5) (CommandTimeYMDHM 2006 7 8 9 10) `shouldBe` Right (localTime 2006 7 8 9 10 0)
      it "returns an error if the given command time is before now" $ do
        commandTimeToLocalTime (localTime 2000 1 2 3 4 0) (CommandTimeYMDHM 2000 1 2 3 3) `shouldBe` Left "past date"
        commandTimeToLocalTime (localTime 2000 1 2 3 4 0) (CommandTimeYMDHM 2000 1 2 3 4) `shouldBe` Left "past date"
        commandTimeToLocalTime (localTime 2000 1 2 3 4 0) (CommandTimeYMDHM 2000 1 2 3 5) `shouldBe` Right (localTime 2000 1 2 3 5 0)
      it "can convert 24:00" $ do
        commandTimeToLocalTime (localTime 2000 1 1 23 59 59) (CommandTimeYMDHM 2000 1 1 24 0) `shouldBe` Right (localTime 2000 1 2 0 0 0)
        commandTimeToLocalTime (localTime 2000 1 2 0 0 0) (CommandTimeYMDHM 2000 1 1 24 0) `shouldBe` Left "past date"
        commandTimeToLocalTime (localTime 2000 1 2 0 0 1) (CommandTimeYMDHM 2000 1 1 24 0) `shouldBe` Left "past date"
      it "returns an error if the given command time has invalid values" $ do
        commandTimeToLocalTime (localTime 2000 1 1 0 0 0) (CommandTimeYMDHM 2000 1 1 0 60) `shouldBe` Left "invalid date"
        commandTimeToLocalTime (localTime 2000 1 1 0 0 0) (CommandTimeYMDHM 2000 1 1 24 1) `shouldBe` Left "invalid date"
        commandTimeToLocalTime (localTime 2000 1 1 0 0 0) (CommandTimeYMDHM 2000 1 1 25 0) `shouldBe` Left "invalid date"
        commandTimeToLocalTime (localTime 2000 1 1 0 0 0) (CommandTimeYMDHM 2000 1 32 0 0) `shouldBe` Left "invalid date"
        commandTimeToLocalTime (localTime 2000 1 1 0 0 0) (CommandTimeYMDHM 2000 13 1 0 0) `shouldBe` Left "invalid date"

localTime :: Integer -> Int -> Int -> Int -> Int -> Pico -> LocalTime
localTime year month day hour minute second = LocalTime (fromGregorian year month day) (TimeOfDay hour minute second)
