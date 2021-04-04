{-# LANGUAGE OverloadedStrings #-}

module Network.ReminderBot.ScheduleStoreSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Either
import Data.Time.Calendar
import Data.Time.Clock
import Network.ReminderBot.Internal.HashCode
import Network.ReminderBot.ScheduleStore
import Test.Hspec
import TestConfig

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let
    timeD d = UTCTime (fromGregorian 10000 1 d) 0
    messageHashCode :: MessageID -> ScheduleID
    messageHashCode = ScheduleID . hashCode . fromIntegral
    brokenConfig = defaultScheduleStoreConfig { mongoHost = "localhost", mongoPort = 0 }

  describe "addSchedule" $ do
    resultAdd <- runIO $ do
      let config = scheduleStoreConfig "collectionAdd"
      addSchedule config $ Schedule (timeD 1) 1 11 101 1001 "message1"
    it "returns a Schedule" $ do
      resultAdd `shouldBe` messageHashCode 1001

  describe "getFirstSchedule" $ do
    resultEmpty <- runIO $ do
      let config = scheduleStoreConfig "collectionGetFirstEmpty"
      getFirstSchedule config
    it "returns Nothing if the DB has no documents" $ do
      resultEmpty `shouldBe` Nothing

    resultMultipleItems <- runIO $ do
      let config = scheduleStoreConfig "collectionGetFirstMultipleItems"
      _ <- addSchedule config $ Schedule (timeD 1) 1 11 101 1001 "message1"
      _ <- addSchedule config $ Schedule (timeD 2) 1 12 202 1002 "message2"
      _ <- addSchedule config $ Schedule (timeD 3) 1 13 303 1003 "message3"
      getFirstSchedule config
    it "returns a schedule which has the most earliest time" $ do
      resultMultipleItems `shouldBe` Just (messageHashCode 1001, Schedule (timeD 1) 1 11 101 1001 "message1")

  describe "getScheduleBeforeOrEqual" $ do
    resultEmpty <- runIO $ do
      let config = scheduleStoreConfig "collectionGetEmpty"
      getScheduleBeforeOrEqual config (timeD 1)
    it "returns an empty list when the DB has no documents" $ do
      resultEmpty `shouldMatchList` []

    (resultSingleItem1, resultSingleItem2) <- runIO $ do
      let config = scheduleStoreConfig "collectionGetSingleItem"
      _ <- addSchedule config $ Schedule (timeD 2) 1 11 101 1001 "message1"
      result1 <- getScheduleBeforeOrEqual config (timeD 1)
      result2 <- getScheduleBeforeOrEqual config (timeD 2)
      return (result1, result2)
    it "returns a schedule whose time is before a specified time" $ do
      resultSingleItem1 `shouldMatchList` []
      resultSingleItem2 `shouldMatchList` [(messageHashCode 1001, Schedule (timeD 2) 1 11 101 1001 "message1")]

    (resultMultipleItems1, resultMultipleItems2, resultMultipleItems3) <- runIO $ do
      let config = scheduleStoreConfig "collectionGetMultipleItems"
      _ <- addSchedule config $ Schedule (timeD 1) 1 11 101 1001 "message1"
      _ <- addSchedule config $ Schedule (timeD 2) 2 12 202 1002 "message2"
      _ <- addSchedule config $ Schedule (timeD 3) 3 13 303 1003 "message3"
      result1 <- getScheduleBeforeOrEqual config (timeD 1)
      result2 <- getScheduleBeforeOrEqual config (timeD 2)
      result3 <- getScheduleBeforeOrEqual config (timeD 3)
      return (result1, result2, result3)
    it "returns all schedules whose time is before the specified time" $ do
      resultMultipleItems1 `shouldMatchList` [(messageHashCode 1001, Schedule (timeD 1) 1 11 101 1001 "message1")]
      resultMultipleItems2 `shouldMatchList` [(messageHashCode 1001, Schedule (timeD 1) 1 11 101 1001 "message1"), (messageHashCode 1002, Schedule (timeD 2) 2 12 202 1002 "message2")]
      resultMultipleItems3 `shouldMatchList` [(messageHashCode 1001, Schedule (timeD 1) 1 11 101 1001 "message1"), (messageHashCode 1002, Schedule (timeD 2) 2 12 202 1002 "message2"), (messageHashCode 1003, Schedule (timeD 3) 3 13 303 1003 "message3")]

    resultManyItems <- runIO $ do
      let config = scheduleStoreConfig "collectionGetManyItems"
      _ <- replicateM_ 200 $ addSchedule config $ Schedule (timeD 1) 1 11 101 1001 "message1"
      getScheduleBeforeOrEqual config (timeD 1)
    it "can return a large number of schedules" $ do
      length resultManyItems `shouldBe` 200

  describe "removeScheduleBeforeOrEqual" $ do
    resultEmpty <- runIO $ do
      let config = scheduleStoreConfig "collectionRemoveEmpty"
      removeScheduleBeforeOrEqual config (timeD 1)
    it "does not crash if the storage is empty" $ do
      resultEmpty `shouldBe` ()

    (resultMultipleItems1, resultMultipleItems2) <- runIO $ do
      let config = scheduleStoreConfig "collectionRemoveMultipleItems"
      _ <- addSchedule config $ Schedule (timeD 1) 1 11 101 1001 "message1"
      _ <- addSchedule config $ Schedule (timeD 2) 2 12 202 1002 "message2"
      _ <- addSchedule config $ Schedule (timeD 3) 3 13 303 1003 "message3"
      result1 <- getScheduleBeforeOrEqual config (timeD 3)
      removeScheduleBeforeOrEqual config (timeD 2)
      result2 <- getScheduleBeforeOrEqual config (timeD 3)
      return (result1, result2)
    it "removes only schedules whose time is before the specified time" $ do
      resultMultipleItems1 `shouldMatchList` [(messageHashCode 1001, Schedule (timeD 1) 1 11 101 1001 "message1"), (messageHashCode 1002, Schedule (timeD 2) 2 12 202 1002 "message2"), (messageHashCode 1003, Schedule (timeD 3) 3 13 303 1003 "message3")]
      resultMultipleItems2 `shouldMatchList` [(messageHashCode 1003, Schedule (timeD 3) 3 13 303 1003 "message3")]

  describe "listGuildSchedule" $ do
    resultEmpty <- runIO $ do
      let config = scheduleStoreConfig "collectionListGuildEmpty"
      listGuildSchedule config 1
    it "returns an empty list when the DB has no documents" $ do
      resultEmpty `shouldBe` []

    (resultMultipleItems1, resultMultipleItems2, resultMultipleItems3, resultMultipleItems4) <- runIO $ do
      let config = scheduleStoreConfig "collectionListGuildMultipleItems"
      _ <- addSchedule config $ Schedule (timeD 1) 1 11 101 1001 "message1"
      _ <- addSchedule config $ Schedule (timeD 1) 2 12 102 2002 "message2"
      _ <- addSchedule config $ Schedule (timeD 2) 1 13 103 1002 "message3"
      result1 <- listGuildSchedule config 1
      result2 <- listGuildSchedule config 2
      result3 <- listGuildSchedule config 3
      result4 <- listGuildSchedule config 1
      return (result1, result2, result3, result4)
    it "returns a schedule list which has the given channel" $ do
      resultMultipleItems1 `shouldBe` [(messageHashCode 1001, Schedule (timeD 1) 1 11 101 1001 "message1"), (messageHashCode 1002, Schedule (timeD 2) 1 13 103 1002 "message3")]
      resultMultipleItems2 `shouldBe` [(messageHashCode 2002, Schedule (timeD 1) 2 12 102 2002 "message2")]
      resultMultipleItems3 `shouldBe` []
      resultMultipleItems4 `shouldBe` [(messageHashCode 1001, Schedule (timeD 1) 1 11 101 1001 "message1"), (messageHashCode 1002, Schedule (timeD 2) 1 13 103 1002 "message3")]

    resultOrdered <- runIO $ do
      let config = scheduleStoreConfig "collectionListGuildOrdered"
      _ <- addSchedule config $ Schedule (timeD 2) 1 13 103 1002 "message3"
      _ <- addSchedule config $ Schedule (timeD 1) 2 12 102 2002 "message2"
      _ <- addSchedule config $ Schedule (timeD 1) 1 11 101 1001 "message1"
      listGuildSchedule config 1
    it "returns an ordered list even if added in reverse order" $ do
      resultOrdered `shouldBe` [(messageHashCode 1001, Schedule (timeD 1) 1 11 101 1001 "message1"), (messageHashCode 1002, Schedule (timeD 2) 1 13 103 1002 "message3")]

  describe "listChannelSchedule" $ do
    resultEmpty <- runIO $ do
      let config = scheduleStoreConfig "collectionListChannelEmpty"
      listChannelSchedule config 11
    it "returns an empty list when the DB has no documents" $ do
      resultEmpty `shouldBe` []

    (resultMultipleItems1, resultMultipleItems2, resultMultipleItems3, resultMultipleItems4) <- runIO $ do
      let config = scheduleStoreConfig "collectionListChannelMultipleItems"
      _ <- addSchedule config $ Schedule (timeD 1) 1 11 101 1001 "message1"
      _ <- addSchedule config $ Schedule (timeD 1) 2 12 102 2002 "message2"
      _ <- addSchedule config $ Schedule (timeD 2) 3 11 103 1002 "message3"
      result1 <- listChannelSchedule config 11
      result2 <- listChannelSchedule config 12
      result3 <- listChannelSchedule config 13
      result4 <- listChannelSchedule config 11
      return (result1, result2, result3, result4)
    it "returns a schedule list which has the given channel" $ do
      resultMultipleItems1 `shouldBe` [(messageHashCode 1001, Schedule (timeD 1) 1 11 101 1001 "message1"), (messageHashCode 1002, Schedule (timeD 2) 3 11 103 1002 "message3")]
      resultMultipleItems2 `shouldBe` [(messageHashCode 2002, Schedule (timeD 1) 2 12 102 2002 "message2")]
      resultMultipleItems3 `shouldBe` []
      resultMultipleItems4 `shouldBe` [(messageHashCode 1001, Schedule (timeD 1) 1 11 101 1001 "message1"), (messageHashCode 1002, Schedule (timeD 2) 3 11 103 1002 "message3")]

    resultOrdered <- runIO $ do
      let config = scheduleStoreConfig "collectionListChannelOrdered"
      _ <- addSchedule config $ Schedule (timeD 2) 3 11 103 1002 "message3"
      _ <- addSchedule config $ Schedule (timeD 1) 2 12 102 2002 "message2"
      _ <- addSchedule config $ Schedule (timeD 1) 1 11 101 1001 "message1"
      listChannelSchedule config 11
    it "returns an ordered list even if added in reverse order" $ do
      resultOrdered `shouldBe` [(messageHashCode 1001, Schedule (timeD 1) 1 11 101 1001 "message1"), (messageHashCode 1002, Schedule (timeD 2) 3 11 103 1002 "message3")]

  describe "removeSchedule" $ do
    (resultRemove1, resultRemove2, resultRemove3, resultRemove4, resultRemove5, resultGet) <- runIO $ do
      let config = scheduleStoreConfig "collectionRemove"
      resultRemove1 <- removeSchedule config (messageHashCode 101)  -- []
      _ <- addSchedule config $ Schedule (timeD 1) 1 11 101 1001 "message1"  -- [1]
      _ <- addSchedule config $ Schedule (timeD 1) 2 12 102 2002 "message2"  -- [1, 2, 3]
      _ <- addSchedule config $ Schedule (timeD 1) 1 13 103 1002 "message3"  -- [1, 2, 3]
      resultRemove2 <- removeSchedule config (messageHashCode 1001)  -- [2, 3]
      resultRemove3 <- removeSchedule config (messageHashCode 1001)  -- [2, 3]
      resultRemove4 <- removeSchedule config (messageHashCode 3003)  -- [2, 3]
      resultRemove5 <- removeSchedule config (messageHashCode 1002)  -- [2]
      resultGet <- getScheduleBeforeOrEqual config (timeD 1)
      return (resultRemove1, resultRemove2, resultRemove3, resultRemove4, resultRemove5, resultGet)
    it "removes a schedule and returns True if and only if the schedule exists" $ do
      resultRemove1 `shouldBe` False
      resultRemove2 `shouldBe` True
      resultRemove3 `shouldBe` False
      resultRemove4 `shouldBe` False
      resultRemove5 `shouldBe` True
      resultGet `shouldMatchList` [(messageHashCode 2002, Schedule (timeD 1) 2 12 102 2002 "message2")]

  describe "trySchedule" $ do
    resultRight <- runIO $ do
      let config = scheduleStoreConfig "collectionTry"
      trySchedule $ addSchedule config $ Schedule (timeD 1) 1 11 101 1001 "message1"
    it "wraps return values in Right" $ do
      resultRight `shouldSatisfy` isRight

    resultLeft <- runIO $ trySchedule $ addSchedule brokenConfig $ Schedule (timeD 1) 1 11 101 1001 "message1"
    it "returns an error and does not crash" $ do
      resultLeft `shouldSatisfy` isLeft

  describe "isUp" $ do
    upResult <- runIO $ isUp $ scheduleStoreConfig "isUp"
    it "returns True" $ do
      upResult `shouldBe` True

    downResult <- runIO $ isUp brokenConfig
    it "returns False" $ do
      downResult `shouldBe` False
