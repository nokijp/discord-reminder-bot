{-# LANGUAGE OverloadedStrings #-}

module Network.ReminderBot.Command.ParserSpec
  ( main
  , spec
  ) where

import Data.Either
import Network.ReminderBot.Command.Parser
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseCommand" $ do
    context "when given an \"add\" command" $ do
      it "can parse \"hh:mm\"" $
        parseCommand "add 11:22 abc" `shouldBe` Right (CommandAdd (CommandTimeHM 11 22) "abc")
      it "can parse \"MM/DD hh:mm\"" $
        parseCommand "add 11/22 33:44 abc" `shouldBe` Right (CommandAdd (CommandTimeMDHM 11 22 33 44) "abc")
      it "can parse \"YYYY/MM/DD hh:mm\"" $
        parseCommand "add 1111/22/33 44:55 abc" `shouldBe` Right (CommandAdd (CommandTimeYMDHM 1111 22 33 44 55) "abc")
      it "can parse a command whose message contains multibyte characters" $
        parseCommand "add 11:22 あいう" `shouldBe` Right (CommandAdd (CommandTimeHM 11 22) "あいう")
      it "can parse a command whose message contains spaces" $
        parseCommand "add 11:22 a b  c" `shouldBe` Right (CommandAdd (CommandTimeHM 11 22) "a b  c")
      it "can parse a command which contains an extra time information" $
        parseCommand "add 11:22 33:44 abc" `shouldBe` Right (CommandAdd (CommandTimeHM 11 22) "33:44 abc")
      it "can parse a command with extra spaces" $
        parseCommand "  add  11:22     abc  " `shouldBe` Right (CommandAdd (CommandTimeHM 11 22) "abc")
      it "returns an error if the body is empty" $
        parseCommand "add" `shouldSatisfy` isLeft
      it "returns an error if the message is empty" $
        parseCommand "add 11:22   " `shouldSatisfy` isLeft

    context "when given a \"ls\" command" $ do
      it "can parse" $
        parseCommand "ls" `shouldBe` Right CommandList
      it "can parse a command with extra spaces" $
        parseCommand "  ls   " `shouldBe` Right CommandList
      it "returns an error if the command has an extra argument" $
        parseCommand "ls a" `shouldSatisfy` isLeft

    context "when given a \"rm\" command" $ do
      it "can parse" $
        parseCommand "rm 1234abc" `shouldBe` Right (CommandRemove 0x1234abc)
      it "can parse a command with extra spaces" $
        parseCommand "    rm   1234abc    " `shouldBe` Right (CommandRemove 0x1234abc)
      it "returns an error if the command has no ID" $
        parseCommand "rm" `shouldSatisfy` isLeft
      it "returns an error if the command has an empty ID" $
        parseCommand "rm " `shouldSatisfy` isLeft
      it "returns an error if the command has an extra argument" $
        parseCommand "rm 1234abc 1234abc" `shouldSatisfy` isLeft

    context "when given an unknown command" $ do
      it "returns CommandHelp" $
        parseCommand "unknown" `shouldBe` Right CommandHelp
      it "returns CommandHelp when given a string which starts with \"add\"" $
        parseCommand "addd 11:22 message" `shouldBe` Right CommandHelp
      it "returns CommandHelp when given a string which starts with \"ls\"" $
        parseCommand "lss" `shouldBe` Right CommandHelp
      it "returns CommandHelp when given a string which starts with \"rm\"" $
        parseCommand "rmm 1234abc" `shouldBe` Right CommandHelp
