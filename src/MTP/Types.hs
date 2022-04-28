{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module MTP.Types
  ( Msg
  , msg
  , hashMsg
  , encodeMsg
  ) where

import           Crypto.Hash          (Digest, SHA256, hash)
import           Data.Aeson           (ToJSON (..), encode, object, (.=))
import           Data.ByteString.Lazy (ByteString, toStrict)
import           Data.String          (fromString)
import           Data.Text.Encoding   (decodeUtf8)
import           Network.MQTT.Topic   (Topic, unTopic)
import           Periodic.Types       (JobName (..), Workload (..))

data Msg = Msg
    { topic   :: Topic
    , payload :: ByteString
    }
    deriving (Show)

instance ToJSON Msg where
  toJSON Msg {..} = object
    [ "topic" .= unTopic topic
    , "payload" .= decodeUtf8 (toStrict payload)
    ]

msg :: Topic -> ByteString -> Msg
msg = Msg

hashMsg_ :: Msg -> Digest SHA256
hashMsg_ = hash . toStrict . encode

hashMsg :: Msg -> JobName
hashMsg = fromString . show . hashMsg_

encodeMsg :: Msg -> Workload
encodeMsg = Workload . toStrict . encode
