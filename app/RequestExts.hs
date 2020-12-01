{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module RequestExts
  ( ExtChannelRequest(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Discord.Types
import Discord.Internal.Rest.Prelude
import Exts
import Network.HTTP.Req

data ExtChannelRequest a where
  CreateReply :: ChannelId -> MessageId -> Text -> ExtChannelRequest Message

instance Request (ExtChannelRequest Message) where
  majorRoute (CreateReply channelID _ _) = "msg " <> show channelID
  jsonRequest (CreateReply channelID messageReferenceID message) = Post url body mempty
    where
      url = baseUrl /: "channels" /: toText channelID /: "messages"
      content = object [ "content" .= message
                       , "message_reference" .= object [ "message_id" .= messageReferenceID ]
                       ]
      body = pure $ ReqBodyJson content
