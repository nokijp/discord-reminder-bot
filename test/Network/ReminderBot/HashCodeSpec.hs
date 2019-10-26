module Network.ReminderBot.HashCodeSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Word
import Network.ReminderBot.HashCode
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "show" $ do
    it "converts HashCode 0 into 00000000" $
      show (HashCode 0x0) `shouldBe` "00000000"
    it "converts HashCode 1 into 00000001" $
      show (HashCode 0x1) `shouldBe` "00000001"
    it "converts HashCode 0xffffffff into ffffffff" $
      show (HashCode 0xffffffff) `shouldBe` "ffffffff"
    it "converts HashCode 0x01234567 into 01234567" $
      show (HashCode 0x01234567) `shouldBe` "01234567"
    it "is the inverse of read" $ forAll hex8 $ \s ->
      show (read s :: HashCode) `shouldBe` s

  describe "shows" $ do
    it "concatenates the second argument with the result of show" $
      shows (HashCode 0x01234567) "xx" `shouldBe` "01234567xx"

  describe "read" $ do
    it "converts 00000000 into HashCode 0" $
      read "00000000" `shouldBe` HashCode 0x0
    it "converts 0123456789abcdef into HashCode 0x89abcdef" $
      read "0123456789abcdef" `shouldBe` HashCode 0x89abcdef
    it "converts 00000001 into HashCode 1" $
      read "00000001" `shouldBe` HashCode 0x1
    it "converts 1 into HashCode 1" $
      read "1" `shouldBe` HashCode 0x1
    it "converts 0xffffffff into HashCode 0xffffffff" $
      read "ffffffff" `shouldBe` HashCode 0xffffffff
    it "converts 0x01234567 into HashCode 0x01234567" $
      read "01234567" `shouldBe` HashCode 0x01234567
    it "is the inverse of show" $ property $ \n ->
      read (show (HashCode n)) `shouldBe` HashCode n

  describe "reads" $ do
    it "reads characters while the character is a hex digit" $
      reads "01234567xx" `shouldBe` [(HashCode 0x01234567, "xx")]

  describe "hashCode" $ do
    it "returns HashCode when given 0 as Word8" $
      hashCode (0 :: Word8) `shouldBe` HashCode 0x6e340b9c
    it "returns HashCode when given 0 as Word64" $
      hashCode (0 :: Word64) `shouldBe` HashCode 0xaf5570f5
    it "returns HashCode when given 1 as Word8" $
      hashCode (1 :: Word8) `shouldBe` HashCode 0x4bf5122f
    it "returns HashCode when given 1 as Word64" $
      hashCode (1 :: Word64) `shouldBe` HashCode 0xcd266215
    it "returns HashCode when given 0x0123456789abcdef as Word64" $
      hashCode (0x0123456789abcdef :: Word64) `shouldBe` HashCode 0x55c53f5d

hex8 :: Gen String
hex8 = replicateM 8 $ elements $ ['0' .. '9'] ++ ['a' .. 'f']
