{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Network.ReminderBot.Command.Parser
  ( module Network.ReminderBot.Command.Time
  , module Network.ReminderBot.Command.Types
  , parseMessage
  ) where

import qualified Control.Applicative as A
import Control.Arrow
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import Network.ReminderBot.Command.Time
import Network.ReminderBot.Command.Types
import Network.ReminderBot.Schedule
import Numeric
import Text.Parsec
import Text.Parsec.Text

parseMessage :: Text -> Text -> Maybe (Either CommandError Command)
parseMessage botUserID message = parseCommand <$> messageBody
  where
    messageBody =     T.stripPrefix ("<@" <> botUserID <> ">") message
                A.<|> T.stripPrefix ("<@!" <> botUserID <> ">") message

parseCommand :: Text -> Either CommandError Command
parseCommand input =
  let (commandName, commandBody) = fromRight ("", "") $ parse inputParser "" input
  in parseCommandBody input commandName commandBody

inputParser :: Parser (Text, Text)
inputParser =   try ((, "") <$> (spaces *> commandName <* spaces <* eof))
            <|>     ((,) <$> (spaces *> commandName) <*> (space *> commandBody <* eof))
  where
    commandName = T.pack <$> many1 alphaNum
    commandBody = T.strip . T.pack <$> many anyToken

parseCommandBody :: Text -> Text -> Text -> Either CommandError Command
parseCommandBody _        "add" commandBody = left (const AddArgumentError) $ parse addCommandParser "" commandBody
parseCommandBody _        "ls"  "all"       = Right CommandListGuild
parseCommandBody _        "ls"  ""          = Right CommandListChannel
parseCommandBody _        "ls"  _           = Left ListArgumentError
parseCommandBody _        "rm"  commandBody = left (const RemoveArgumentError) $ parse removeCommandParser "" commandBody
parseCommandBody rawInput _     _           = left (const UnknownCommandError) $ parse addCommandParser "" $ T.stripStart rawInput

addCommandParser :: Parser Command
addCommandParser = CommandAdd <$> timeParser <*> (skipMany1 space *> messageParser <* eof)
  where
    messageParser :: Parser Text
    messageParser = T.strip . T.pack <$> many1 anyChar
    timeParser :: Parser CommandTime
    timeParser =   try (CommandTimeHM <$> n <*> (char ':' *> n))
               <|> try (CommandTimeMDHM <$> n <*> (char '/' *> n) <*> (char ' ' *> n) <*> (char ':' *> n))
               <|>     (CommandTimeYMDHM <$> n <*> (char '/' *> n) <*> (char '/' *> n) <*> (char ' ' *> n) <*> (char ':' *> n))
    n :: Read a => Parser a
    n = read <$> many1 digit

removeCommandParser :: Parser Command
removeCommandParser = CommandRemove <$> (sourceParser <* eof)
  where
    sourceParser :: Parser ScheduleID
    sourceParser = ScheduleID . forceReadHex <$> many1 hexDigit
    forceReadHex :: (Eq a, Num a) => String -> a
    forceReadHex s = let [(n, "")] = readHex s in n
