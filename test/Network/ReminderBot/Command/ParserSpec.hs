{-# LANGUAGE OverloadedStrings #-}

module Network.ReminderBot.Command.ParserSpec
  ( main
  , spec
  ) where

import Network.ReminderBot.Command.Parser
import Network.ReminderBot.Schedule
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseMessage" $ do
    context "when given an \"add\" command" $ do
      --
      -- when you modify this context section, make the same change to the next context section.
      --
      it "can parse \"hh:mm\"" $
        parseMessage "1234" "<@1234>add 11:22 abc" `shouldBe` Just (Right (CommandAdd (CommandTimeHM 11 22) "abc"))
      it "can parse \"MM/DD hh:mm\"" $
        parseMessage "1234" "<@1234>add 11/22 33:44 abc" `shouldBe` Just (Right (CommandAdd (CommandTimeMDHM 11 22 33 44) "abc"))
      it "can parse \"YYYY/MM/DD hh:mm\"" $
        parseMessage "1234" "<@1234>add 1111/22/33 44:55 abc" `shouldBe` Just (Right (CommandAdd (CommandTimeYMDHM 1111 22 33 44 55) "abc"))
      it "can parse a command whose message contains multibyte characters" $
        parseMessage "1234" "<@1234>add 11:22 あいう" `shouldBe` Just (Right (CommandAdd (CommandTimeHM 11 22) "あいう"))
      it "can parse a command whose message contains spaces" $
        parseMessage "1234" "<@1234>add 11:22 a b  c" `shouldBe` Just (Right (CommandAdd (CommandTimeHM 11 22) "a b  c"))
      it "can parse a command which contains an extra time information" $
        parseMessage "1234" "<@1234>add 11:22 33:44 abc" `shouldBe` Just (Right (CommandAdd (CommandTimeHM 11 22) "33:44 abc"))
      it "can parse a command with extra spaces" $
        parseMessage "1234" "<@1234>  add  11:22     abc  " `shouldBe` Just (Right (CommandAdd (CommandTimeHM 11 22) "abc"))
      it "returns AddArgumentError if the body is empty" $
        parseMessage "1234" "<@1234>add" `shouldBe` Just (Left AddArgumentError)
      it "returns AddArgumentError if the message is empty" $
        parseMessage "1234" "<@1234>add 11:22   " `shouldBe` Just (Left AddArgumentError)
      it "can parse nicknames" $
        parseMessage "1234" "<@!1234>add 11:22 abc" `shouldBe` Just (Right (CommandAdd (CommandTimeHM 11 22) "abc"))
      it "returns Nothing if the ID does not match" $
        parseMessage "1234" "<@5678>add 11:22 abd" `shouldBe` Nothing

    context "when given an implicit \"add\" command" $ do
      it "can parse \"hh:mm\"" $
        parseMessage "1234" "<@1234>11:22 abc" `shouldBe` Just (Right (CommandAdd (CommandTimeHM 11 22) "abc"))
      it "can parse \"MM/DD hh:mm\"" $
        parseMessage "1234" "<@1234>11/22 33:44 abc" `shouldBe` Just (Right (CommandAdd (CommandTimeMDHM 11 22 33 44) "abc"))
      it "can parse \"YYYY/MM/DD hh:mm\"" $
        parseMessage "1234" "<@1234>1111/22/33 44:55 abc" `shouldBe` Just (Right (CommandAdd (CommandTimeYMDHM 1111 22 33 44 55) "abc"))
      it "can parse a command whose message contains multibyte characters" $
        parseMessage "1234" "<@1234>11:22 あいう" `shouldBe` Just (Right (CommandAdd (CommandTimeHM 11 22) "あいう"))
      it "can parse a command whose message contains spaces" $
        parseMessage "1234" "<@1234>11:22 a b  c" `shouldBe` Just (Right (CommandAdd (CommandTimeHM 11 22) "a b  c"))
      it "can parse a command which contains an extra time information" $
        parseMessage "1234" "<@1234>11:22 33:44 abc" `shouldBe` Just (Right (CommandAdd (CommandTimeHM 11 22) "33:44 abc"))
      it "can parse a command with extra spaces" $
        parseMessage "1234" "<@1234>  11:22     abc  " `shouldBe` Just (Right (CommandAdd (CommandTimeHM 11 22) "abc"))
      it "returns AddArgumentError if the body is empty" $
        parseMessage "1234" "<@1234>" `shouldBe` Just (Left UnknownCommandError)
      it "returns AddArgumentError if the message is empty" $
        parseMessage "1234" "<@1234>11:22   " `shouldBe` Just (Left UnknownCommandError)
      it "can parse nicknames" $
        parseMessage "1234" "<@!1234>11:22 abc" `shouldBe` Just (Right (CommandAdd (CommandTimeHM 11 22) "abc"))
      it "returns Nothing if the ID does not match" $
        parseMessage "1234" "<@5678>11:22 abd" `shouldBe` Nothing

    context "when given a \"ls\" command" $ do
      it "can parse" $
        parseMessage "1234" "<@1234>ls" `shouldBe` Just (Right CommandListChannel)
      it "can parse a command with an option" $
        parseMessage "1234" "<@1234>ls all" `shouldBe` Just (Right CommandListGuild)
      it "can parse a command with extra spaces" $
        parseMessage "1234" "<@1234>  ls   " `shouldBe` Just (Right CommandListChannel)
      it "can parse a command with an option and extra spaces" $
        parseMessage "1234" "<@1234>  ls   all   " `shouldBe` Just (Right CommandListGuild)
      it "returns ListArgumentError if the command has an unexpected argument" $
        parseMessage "1234" "<@1234>ls xxx" `shouldBe` Just (Left ListArgumentError)
      it "can parse nicknames" $
        parseMessage "1234" "<@!1234>ls" `shouldBe` Just (Right CommandListChannel)
      it "returns Nothing if the ID does not match" $
        parseMessage "1234" "<@5678>ls" `shouldBe` Nothing

    context "when given a \"rm\" command" $ do
      it "can parse" $
        parseMessage "1234" "<@1234>rm 1234abc" `shouldBe` Just (Right (CommandRemove $ ScheduleID 0x1234abc))
      it "can parse a command with extra spaces" $
        parseMessage "1234" "<@1234>    rm   1234abc    " `shouldBe` Just (Right (CommandRemove $ ScheduleID 0x1234abc))
      it "can parse a number which starts with 0" $
        parseMessage "1234" "<@1234>rm 0000" `shouldBe` Just (Right (CommandRemove $ ScheduleID 0))
      it "does not crash when given a large number" $
        parseMessage "1234" "<@1234>rm ffffffffffffffffffffffffffffffffffffffff" `shouldBe` Just (Right (CommandRemove $ ScheduleID 0xffffffffffffffff))
      it "returns RemoveArgumentError if the command has no ID" $
        parseMessage "1234" "<@1234>rm" `shouldBe` Just (Left RemoveArgumentError)
      it "returns RemoveArgumentError if the command has an empty ID" $
        parseMessage "1234" "<@1234>rm " `shouldBe` Just (Left RemoveArgumentError)
      it "returns RemoveArgumentError if the command has an extra argument" $
        parseMessage "1234" "<@1234>rm 1234abc 1234abc" `shouldBe` Just (Left RemoveArgumentError)
      it "can parse nicknames" $
        parseMessage "1234" "<@!1234>rm 1234abc" `shouldBe` Just (Right (CommandRemove $ ScheduleID 0x1234abc))
      it "returns Nothing if the ID does not match" $
        parseMessage "1234" "<@5678>rm 1234abc" `shouldBe` Nothing

    context "when given an unknown command" $ do
      it "returns UnknownCommandError" $
        parseMessage "1234" "<@1234>unknown" `shouldBe` Just (Left UnknownCommandError)
      it "returns UnknownCommandError when given a string which starts with \"add\"" $
        parseMessage "1234" "<@1234>addd 11:22 message" `shouldBe` Just (Left UnknownCommandError)
      it "returns UnknownCommandError when given a string which starts with \"ls\"" $
        parseMessage "1234" "<@1234>lss" `shouldBe` Just (Left UnknownCommandError)
      it "returns UnknownCommandError when given a string which starts with \"rm\"" $
        parseMessage "1234" "<@1234>rmm 1234abc" `shouldBe` Just (Left UnknownCommandError)
      it "can parse nicknames" $
        parseMessage "1234" "<@!1234>unknown" `shouldBe` Just (Left UnknownCommandError)
      it "returns Nothing if the ID does not match" $
        parseMessage "1234" "<@5678>unknown" `shouldBe` Nothing
