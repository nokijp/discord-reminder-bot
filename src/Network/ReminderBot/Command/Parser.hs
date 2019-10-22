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
import Data.Word
import Network.ReminderBot.Command.Types
import Numeric
import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.Text

parseCommand :: Text -> Either Text Command
parseCommand input =
  let (commandName, commandBody) = fromRight ("", "") $ parse inputParser "" input
  in left errorMessage $ parse (commandParser commandName) "" commandBody

inputParser :: Parser (Text, Text)
inputParser =   try ((, "") <$> (spaces *> commandName <* spaces <* eof))
            <|>     ((,) <$> (spaces *> commandName) <*> (space *> commandBody <* eof))
  where
    commandName = T.pack <$> many1 alphaNum
    commandBody = T.strip . T.pack <$> many anyToken

commandParser :: Text -> Parser Command
commandParser "add" = addCommandParser
commandParser "ls"  = listCommandParser
commandParser "rm"  = removeCommandParser
commandParser _     = CommandHelp <$ many anyChar

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

listCommandParser :: Parser Command
listCommandParser = CommandList <$ eof

removeCommandParser :: Parser Command
removeCommandParser = CommandRemove <$> (sourceParser <* eof)
  where
    sourceParser :: Parser Word64
    sourceParser = forceReadHex <$> many1 hexDigit
    forceReadHex :: (Eq a, Num a) => String -> a
    forceReadHex s = let [(n, "")] = readHex s in n

errorMessage :: ParseError -> Text
errorMessage = T.pack . showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" . errorMessages
