{-# LANGUAGE OverloadedStrings #-}

module Network.ReminderBot.ScheduleStoreSpec
  ( main
  , spec
  ) where

import Data.Time.Calendar
import Data.Time.Clock
import Network.ReminderBot.HashCode
import Network.ReminderBot.Schedule
import Network.ReminderBot.ScheduleStore
import Test.Hspec
import TestConfig

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let
    time1 = UTCTime (fromGregorian 10000 1 1) 0
    time2 = UTCTime (fromGregorian 10000 1 2) 0
    time3 = UTCTime (fromGregorian 10000 1 3) 0
    channelID1 = 1
    messageID1 = 101
    message1 = "message1"
    schedule1 = Schedule { scheduleChannel = channelID1
                         , scheduleIdentifier = messageHashCode messageID1
                         , scheduleMessage = message1
                         }
    channelID2 = 2
    messageID2 = 201
    message2 = "message2"
    schedule2 = Schedule { scheduleChannel = channelID2
                         , scheduleIdentifier = messageHashCode messageID2
                         , scheduleMessage = message2
                         }
    channelID3 = 3
    messageID3 = 301
    message3 = "message3"
    schedule3 = Schedule { scheduleChannel = channelID3
                         , scheduleIdentifier = messageHashCode messageID3
                         , scheduleMessage = message3
                         }
    channelID4 = 1
    messageID4 = 102
    message4 = "message4"
    schedule4 = Schedule { scheduleChannel = channelID4
                         , scheduleIdentifier = messageHashCode messageID4
                         , scheduleMessage = message4
                         }

  describe "addSchedule" $ do
    resultAdd <- runIO $ do
      let config = scheduleStoreConfig "collectionAdd"
      addSchedule config time1 channelID1 messageID1 message1
    it "returns a Schedule" $ do
      resultAdd `shouldBe` schedule1

  describe "getFirstSchedule" $ do
    resultEmpty <- runIO $ do
      let config = scheduleStoreConfig "collectionGetFirstEmpty"
      getFirstSchedule config
    it "returns Nothing if the DB has no documents" $ do
      resultEmpty `shouldBe` Nothing

    resultMultipleItems <- runIO $ do
      let config = scheduleStoreConfig "collectionGetFirstMultipleItems"
      _ <- addSchedule config time1 channelID1 messageID1 message1
      _ <- addSchedule config time2 channelID2 messageID2 message2
      _ <- addSchedule config time3 channelID3 messageID3 message3
      getFirstSchedule config
    it "returns a schedule which has the most earliest time" $ do
      resultMultipleItems `shouldBe` Just (time1, schedule1)

  describe "getScheduleBefore" $ do
    resultEmpty <- runIO $ do
      let config = scheduleStoreConfig "collectionGetEmpty"
      getScheduleBefore config time1
    it "returns an empty list when the DB has no documents" $ do
      resultEmpty `shouldMatchList` []

    (resultSingleItem1, resultSingleItem2) <- runIO $ do
      let config = scheduleStoreConfig "collectionGetSingleItem"
      _ <- addSchedule config time2 channelID1 messageID1 message1
      result1 <- getScheduleBefore config time1
      result2 <- getScheduleBefore config time2
      return (result1, result2)
    it "returns a schedule whose time is before a specified time" $ do
      resultSingleItem1 `shouldMatchList` []
      resultSingleItem2 `shouldMatchList` [schedule1]

    (resultMultipleItems1, resultMultipleItems2, resultMultipleItems3) <- runIO $ do
      let config = scheduleStoreConfig "collectionGetMultipleItems"
      _ <- addSchedule config time1 channelID1 messageID1 message1
      _ <- addSchedule config time2 channelID2 messageID2 message2
      _ <- addSchedule config time3 channelID3 messageID3 message3
      result1 <- getScheduleBefore config time1
      result2 <- getScheduleBefore config time2
      result3 <- getScheduleBefore config time3
      return (result1, result2, result3)
    it "returns all schedules whose time is before the specified time" $ do
      resultMultipleItems1 `shouldMatchList` [schedule1]
      resultMultipleItems2 `shouldMatchList` [schedule1, schedule2]
      resultMultipleItems3 `shouldMatchList` [schedule1, schedule2, schedule3]

  describe "removeScheduleBefore" $ do
    resultEmpty <- runIO $ do
      let config = scheduleStoreConfig "collectionRemoveEmpty"
      removeScheduleBefore config time1
    it "does not crash if the storage is empty" $ do
      resultEmpty `shouldBe` ()

    (resultMultipleItems1, resultMultipleItems2) <- runIO $ do
      let config = scheduleStoreConfig "collectionRemoveMultipleItems"
      _ <- addSchedule config time1 channelID1 messageID1 message1
      _ <- addSchedule config time2 channelID2 messageID2 message2
      _ <- addSchedule config time3 channelID3 messageID3 message3
      result1 <- getScheduleBefore config time3
      removeScheduleBefore config time2
      result2 <- getScheduleBefore config time3
      return (result1, result2)
    it "removes only schedules whose time is before the specified time" $ do
      resultMultipleItems1 `shouldMatchList` [schedule1, schedule2, schedule3]
      resultMultipleItems2 `shouldMatchList` [schedule3]

  describe "listSchedule" $ do
    resultEmpty <- runIO $ do
      let config = scheduleStoreConfig "collectionListEmpty"
      listSchedule config 1
    it "returns an empty list when the DB has no documents" $ do
      resultEmpty `shouldBe` []

    (resultMultipleItems1, resultMultipleItems2, resultMultipleItems3, resultMultipleItems4) <- runIO $ do
      let config = scheduleStoreConfig "collectionListMultipleItems"
      _ <- addSchedule config time1 channelID1 messageID1 message1
      _ <- addSchedule config time1 channelID2 messageID2 message2
      _ <- addSchedule config time2 channelID4 messageID4 message4
      result1 <- listSchedule config 1
      result2 <- listSchedule config 2
      result3 <- listSchedule config 3
      result4 <- listSchedule config 1
      return (result1, result2, result3, result4)
    it "returns a schedule list which has the given channel" $ do
      resultMultipleItems1 `shouldBe` [(time1, schedule1), (time2, schedule4)]
      resultMultipleItems2 `shouldBe` [(time1, schedule2)]
      resultMultipleItems3 `shouldBe` []
      resultMultipleItems4 `shouldBe` [(time1, schedule1), (time2, schedule4)]

    resultOrdered <- runIO $ do
      let config = scheduleStoreConfig "collectionListOrdered"
      _ <- addSchedule config time2 channelID4 messageID4 message4
      _ <- addSchedule config time1 channelID2 messageID2 message2
      _ <- addSchedule config time1 channelID1 messageID1 message1
      listSchedule config 1
    it "returns an ordered list even if added in reverse order" $ do
      resultOrdered `shouldBe` [(time1, schedule1), (time2, schedule4)]

  describe "removeSchedule" $ do
    (resultRemove1, resultRemove2, resultRemove3, resultRemove4, resultRemove5, resultRemove6, resultGet) <- runIO $ do
      let config = scheduleStoreConfig "collectionRemove"
      resultRemove1 <- removeSchedule config channelID1 (messageHashCode messageID1)  -- []
      _ <- addSchedule config time1 channelID1 messageID1 message1  -- [1]
      _ <- addSchedule config time1 channelID2 messageID2 message2  -- [1, 2, 4]
      _ <- addSchedule config time1 channelID4 messageID4 message4  -- [1, 2, 4]
      resultRemove2 <- removeSchedule config channelID1 (messageHashCode messageID1)  -- [2, 4]
      resultRemove3 <- removeSchedule config channelID1 (messageHashCode messageID1)  -- [2, 4]
      resultRemove4 <- removeSchedule config channelID3 (messageHashCode messageID3)  -- [2, 4]
      resultRemove5 <- removeSchedule config channelID4 (messageHashCode messageID2)  -- [2, 4]
      resultRemove6 <- removeSchedule config channelID2 (messageHashCode messageID4)  -- [2, 4]
      resultGet <- getScheduleBefore config time1
      return (resultRemove1, resultRemove2, resultRemove3, resultRemove4, resultRemove5, resultRemove6, resultGet)
    it "removes a schedule and returns True if and only if the schedule exists" $ do
      resultRemove1 `shouldBe` False
      resultRemove2 `shouldBe` True
      resultRemove3 `shouldBe` False
      resultRemove4 `shouldBe` False
      resultRemove5 `shouldBe` False
      resultRemove6 `shouldBe` False
      resultGet `shouldMatchList` [schedule2, schedule4]

messageHashCode :: MessageID -> HashCode
messageHashCode = hashCode
