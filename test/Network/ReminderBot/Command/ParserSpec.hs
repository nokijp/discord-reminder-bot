{-# LANGUAGE OverloadedStrings #-}

module Network.ReminderBot.Command.ParserSpec
  ( main
  , spec
  ) where

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
      it "returns AddArgumentError if the body is empty" $
        parseCommand "add" `shouldBe` Left AddArgumentError
      it "returns AddArgumentError if the message is empty" $
        parseCommand "add 11:22   " `shouldBe` Left AddArgumentError

    context "when given a \"ls\" command" $ do
      it "can parse" $
        parseCommand "ls" `shouldBe` Right CommandListChannel
      it "can parse a command with an option" $
        parseCommand "ls all" `shouldBe` Right CommandListGuild
      it "can parse a command with extra spaces" $
        parseCommand "  ls   " `shouldBe` Right CommandListChannel
      it "can parse a command with an option and extra spaces" $
        parseCommand "  ls   all   " `shouldBe` Right CommandListGuild
      it "returns ListArgumentError if the command has an unexpected argument" $
        parseCommand "ls xxx" `shouldBe` Left ListArgumentError

    context "when given a \"rm\" command" $ do
      it "can parse" $
        parseCommand "rm 1234abc" `shouldBe` Right (CommandRemove 0x1234abc)
      it "can parse a command with extra spaces" $
        parseCommand "    rm   1234abc    " `shouldBe` Right (CommandRemove 0x1234abc)
      it "can parse a number which starts with 0" $
        parseCommand "rm 0000" `shouldBe` Right (CommandRemove 0)
      it "does not crash when given a large number" $
        parseCommand "rm ffffffffffffffffffffffffffffffffffffffff" `shouldBe` Right (CommandRemove 0xffffffff)
      it "returns RemoveArgumentError if the command has no ID" $
        parseCommand "rm" `shouldBe` Left RemoveArgumentError
      it "returns RemoveArgumentError if the command has an empty ID" $
        parseCommand "rm " `shouldBe` Left RemoveArgumentError
      it "returns RemoveArgumentError if the command has an extra argument" $
        parseCommand "rm 1234abc 1234abc" `shouldBe` Left RemoveArgumentError

    context "when given an unknown command" $ do
      it "returns UnknownCommandError" $
        parseCommand "unknown" `shouldBe` Left UnknownCommandError
      it "returns UnknownCommandError when given a string which starts with \"add\"" $
        parseCommand "addd 11:22 message" `shouldBe` Left UnknownCommandError
      it "returns UnknownCommandError when given a string which starts with \"ls\"" $
        parseCommand "lss" `shouldBe` Left UnknownCommandError
      it "returns UnknownCommandError when given a string which starts with \"rm\"" $
        parseCommand "rmm 1234abc" `shouldBe` Left UnknownCommandError
