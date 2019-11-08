{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Network.ReminderBot.Command.Parser
  ( module Network.ReminderBot.Command.Types
  , parseCommand
  ) where

import Control.Arrow
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import Network.ReminderBot.Command.Types
import Network.ReminderBot.HashCode
import Numeric
import Text.Parsec
import Text.Parsec.Text

parseCommand :: Text -> Either CommandError Command
parseCommand input =
  let (commandName, commandBody) = fromRight ("", "") $ parse inputParser "" input
  in parseCommandBody commandName commandBody

inputParser :: Parser (Text, Text)
inputParser =   try ((, "") <$> (spaces *> commandName <* spaces <* eof))
            <|>     ((,) <$> (spaces *> commandName) <*> (space *> commandBody <* eof))
  where
    commandName = T.pack <$> many1 alphaNum
    commandBody = T.strip . T.pack <$> many anyToken

parseCommandBody :: Text -> Text -> Either CommandError Command
parseCommandBody "add" commandBody = left (const AddArgumentError) $ parse addCommandParser "" commandBody
parseCommandBody "ls"  ""          = Right CommandList
parseCommandBody "ls"  _           = Left ListArgumentError
parseCommandBody "rm"  commandBody = left (const RemoveArgumentError) $ parse removeCommandParser "" commandBody
parseCommandBody _     _           = Left UnknownCommandError

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
    sourceParser :: Parser HashCode
    sourceParser = forceReadHex <$> many1 hexDigit
    forceReadHex :: (Eq a, Num a) => String -> a
    forceReadHex s = let [(n, "")] = readHex s in n
