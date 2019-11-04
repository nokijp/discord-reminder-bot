{-# LANGUAGE OverloadedStrings #-}

module Network.ReminderBot.Command.TypesSpec
  ( main
  , spec
  ) where

import Data.Fixed
import Data.Time.Calendar
import Data.Time.LocalTime
import Network.ReminderBot.Command.Types
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "commandTimeToLocalTime" $ do
    context "when given CommandTimeHM" $ do
      it "modifies hours and minutes, and set seconds to 0" $
        commandTimeToLocalTime (localTime 2000 1 2 3 4 5) (CommandTimeHM 6 7) `shouldBe` Just (localTime 2000 1 2 6 7 0)
      it "returns Nothing if the given command time is before now" $ do
        commandTimeToLocalTime (localTime 2000 1 2 3 4 0) (CommandTimeHM 3 3) `shouldBe` Nothing
        commandTimeToLocalTime (localTime 2000 1 2 3 4 0) (CommandTimeHM 3 4) `shouldBe` Nothing
        commandTimeToLocalTime (localTime 2000 1 2 3 4 0) (CommandTimeHM 3 5) `shouldBe` Just (localTime 2000 1 2 3 5 0)
      it "returns Nothing if the given command time has invalid values" $ do
        commandTimeToLocalTime (localTime 2000 1 1 0 0 0) (CommandTimeHM 0 60) `shouldBe` Nothing
        commandTimeToLocalTime (localTime 2000 1 1 0 0 0) (CommandTimeHM 25 0) `shouldBe` Nothing

    context "when given CommandTimeMDHM" $ do
      it "modifies month, day, hours and minutes, and set seconds to 0" $
        commandTimeToLocalTime (localTime 2000 1 2 3 4 5) (CommandTimeMDHM 6 7 8 9) `shouldBe` Just (localTime 2000 6 7 8 9 0)
      it "returns Nothing if the given command time is before now" $ do
        commandTimeToLocalTime (localTime 2000 1 2 3 4 0) (CommandTimeMDHM 1 2 3 3) `shouldBe` Nothing
        commandTimeToLocalTime (localTime 2000 1 2 3 4 0) (CommandTimeMDHM 1 2 3 4) `shouldBe` Nothing
        commandTimeToLocalTime (localTime 2000 1 2 3 4 0) (CommandTimeMDHM 1 2 3 5) `shouldBe` Just (localTime 2000 1 2 3 5 0)
      it "returns Nothing if the given command time has invalid values" $ do
        commandTimeToLocalTime (localTime 2000 1 1 0 0 0) (CommandTimeMDHM 1 1 0 60) `shouldBe` Nothing
        commandTimeToLocalTime (localTime 2000 1 1 0 0 0) (CommandTimeMDHM 1 1 25 0) `shouldBe` Nothing
        commandTimeToLocalTime (localTime 2000 1 1 0 0 0) (CommandTimeMDHM 1 32 0 0) `shouldBe` Nothing
        commandTimeToLocalTime (localTime 2000 1 1 0 0 0) (CommandTimeMDHM 13 1 0 0) `shouldBe` Nothing

    context "when given CommandTimeYMDHM" $ do
      it "modifies year, month, day, hours and minutes, and set seconds to 0" $
        commandTimeToLocalTime (localTime 2000 1 2 3 4 5) (CommandTimeYMDHM 2006 7 8 9 10) `shouldBe` Just (localTime 2006 7 8 9 10 0)
      it "returns Nothing if the given command time has invalid values" $ do
        commandTimeToLocalTime (localTime 2000 1 1 0 0 0) (CommandTimeYMDHM 2000 1 1 0 60) `shouldBe` Nothing
        commandTimeToLocalTime (localTime 2000 1 1 0 0 0) (CommandTimeYMDHM 2000 1 1 25 0) `shouldBe` Nothing
        commandTimeToLocalTime (localTime 2000 1 1 0 0 0) (CommandTimeYMDHM 2000 1 32 0 0) `shouldBe` Nothing
        commandTimeToLocalTime (localTime 2000 1 1 0 0 0) (CommandTimeYMDHM 2000 13 1 0 0) `shouldBe` Nothing

localTime :: Integer -> Int -> Int -> Int -> Int -> Pico -> LocalTime
localTime year month day hour minute second = LocalTime (fromGregorian year month day) (TimeOfDay hour minute second)
