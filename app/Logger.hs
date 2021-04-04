{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger
  ( putLog
  , putLog'
  ) where

import Control.Monad.IO.Class
import Data.Time.LocalTime
import Language.Haskell.TH
import System.Log.FastLogger

putLog :: (MonadIO m, ToLogStr s) => LoggerSet -> String -> Int -> s -> m ()
putLog logset modulePath fileLine message = liftIO $ do
  now <- getZonedTime
  pushLogStrLn logset $ toLogStr (show now) <> " [" <> toLogStr modulePath <> ":" <> toLogStr fileLine <> "]: " <> toLogStr message

putLog' :: ExpQ
putLog' = do
  loc <- location
  let
    modulePath = litE $ StringL $ loc_module loc
    fileLine = litE $ IntegerL $ toInteger $ fst $ loc_start loc
  [|\logset -> putLog logset $modulePath $fileLine|]
