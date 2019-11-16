module Network.ReminderBot.HashCodeSpec
  ( main
  , spec
  ) where

import Control.Monad
import Network.ReminderBot.HashCode
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "show" $ do
    it "converts HashCode 0 into 0000000000000000" $
      show (HashCode 0x0) `shouldBe` "0000000000000000"
    it "converts HashCode 1 into 0000000000000001" $
      show (HashCode 0x1) `shouldBe` "0000000000000001"
    it "converts HashCode 0xffffffffffffffff into ffffffffffffffff" $
      show (HashCode 0xffffffffffffffff) `shouldBe` "ffffffffffffffff"
    it "converts HashCode 0x0123456789abcdef into 0123456789abcdef" $
      show (HashCode 0x0123456789abcdef) `shouldBe` "0123456789abcdef"
    it "is the inverse of read" $ forAll hex16 $ \s ->
      show (read s :: HashCode) `shouldBe` s

  describe "shows" $ do
    it "concatenates the second argument with the result of show" $
      shows (HashCode 0x0123456789abcdef) "xx" `shouldBe` "0123456789abcdefxx"

  describe "read" $ do
    it "converts 0000000000000000 into HashCode 0" $
      read "0000000000000000" `shouldBe` HashCode 0x0
    it "converts ffff0123456789abcdef into HashCode 0x0123456789abcdef" $
      read "ffff0123456789abcdef" `shouldBe` HashCode 0x0123456789abcdef
    it "converts 0000000000000001 into HashCode 1" $
      read "0000000000000001" `shouldBe` HashCode 0x1
    it "converts 1 into HashCode 1" $
      read "1" `shouldBe` HashCode 0x1
    it "converts 0xffffffffffffffff into HashCode 0xffffffffffffffff" $
      read "ffffffffffffffff" `shouldBe` HashCode 0xffffffffffffffff
    it "converts 0x0123456789abcdef into HashCode 0x0123456789abcdef" $
      read "0123456789abcdef" `shouldBe` HashCode 0x0123456789abcdef
    it "is the inverse of show" $ property $ \n ->
      read (show (HashCode n)) `shouldBe` HashCode n

  describe "reads" $ do
    it "reads characters while the character is a hex digit" $
      reads "0123456789abcdefxx" `shouldBe` [(HashCode 0x0123456789abcdef, "xx")]

  describe "hashCode" $ do
    it "returns HashCode when given 0" $
      hashCode 0 `shouldBe` HashCode 0
    it "returns HashCode when given 1" $
      hashCode 1 `shouldBe` HashCode 11400714819323198549
    it "returns HashCode when given 0x0123456789abcdef" $
      hashCode 0x0123456789abcdef `shouldBe` HashCode 0x5565019a05e1245b

  describe "hashCodeInv" $ do
    it "is the inverse of hashCode" $ property $ \n ->
      hashCodeInv (hashCode n) `shouldBe` n

hex16 :: Gen String
hex16 = replicateM 16 $ elements $ ['0' .. '9'] ++ ['a' .. 'f']
