{-# LANGUAGE OverloadedStrings #-}

module Network.ReminderBot.ScheduleStoreSpec
  ( main
  , spec
  ) where

import Data.Time.Calendar
import Data.Time.Clock
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
    schedule1 = Schedule { scheduleChannel = 1
                         , scheduleSource = 101
                         , scheduleMessage = "message1"
                         }
    schedule2 = Schedule { scheduleChannel = 2
                         , scheduleSource = 201
                         , scheduleMessage = "message2"
                         }
    schedule3 = Schedule { scheduleChannel = 3
                         , scheduleSource = 301
                         , scheduleMessage = "message3"
                         }
    schedule4 = Schedule { scheduleChannel = 1
                         , scheduleSource = 102
                         , scheduleMessage = "message4"
                         }

  describe "getSchedule" $ do
    resultEmpty <- runIO $ do
      let config = scheduleStoreConfig "collectionGetEmpty"
      getSchedule config time1
    it "returns an empty list when the DB has no documents" $ do
      resultEmpty `shouldMatchList` []

    (resultSingleItem1, resultSingleItem2, resultSingleItem3) <- runIO $ do
      let config = scheduleStoreConfig "collectionGetSingleItem"
      addSchedule config time1 schedule1
      result1 <- getSchedule config time2
      result2 <- getSchedule config time1
      result3 <- getSchedule config time1
      return (result1, result2, result3)
    it "returns a list only for the first query" $ do
      resultSingleItem1 `shouldMatchList` []
      resultSingleItem2 `shouldMatchList` [schedule1]
      resultSingleItem3 `shouldMatchList` []

    (resultMultipleItems1, resultMultipleItems2) <- runIO $ do
      let config = scheduleStoreConfig "collectionGetMultipleItems"
      addSchedule config time1 schedule1
      addSchedule config time1 schedule2
      addSchedule config time2 schedule3
      result1 <- getSchedule config time1
      result2 <- getSchedule config time2
      return (result1, result2)
    it "returns a list only for the first query" $ do
      resultMultipleItems1 `shouldMatchList` [schedule1, schedule2]
      resultMultipleItems2 `shouldMatchList` [schedule3]

  describe "listSchedule" $ do
    resultEmpty <- runIO $ do
      let config = scheduleStoreConfig "collectionListEmpty"
      listSchedule config 1
    it "returns an empty list when the DB has no documents" $ do
      resultEmpty `shouldBe` []

    (resultMultipleItems1, resultMultipleItems2, resultMultipleItems3, resultMultipleItems4) <- runIO $ do
      let config = scheduleStoreConfig "collectionListMultipleItems"
      addSchedule config time1 schedule1
      addSchedule config time1 schedule2
      addSchedule config time2 schedule4
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
      addSchedule config time2 schedule4
      addSchedule config time1 schedule2
      addSchedule config time1 schedule1
      listSchedule config 1
    it "returns an ordered list even if added in reverse order" $ do
      resultOrdered `shouldBe` [(time1, schedule1), (time2, schedule4)]
