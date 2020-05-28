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
import           Data.Byteable        (toBytes)
import           Data.ByteString.Lazy (ByteString, toStrict)
import           Data.Text.Encoding   (decodeUtf8)
import           Network.MQTT.Client  (Topic)
import           Periodic.Types       (JobName (..), Workload (..))

data Msg = Msg
    { topic   :: Topic
    , payload :: ByteString
    }
    deriving (Show)

instance ToJSON Msg where
  toJSON Msg {..} = object [ "topic" .= topic, "payload" .= decodeUtf8 (toStrict payload) ]

msg :: Topic -> ByteString -> Msg
msg = Msg

hashMsg_ :: Msg -> Digest SHA256
hashMsg_ = hash . toStrict . encode

hashMsg :: Msg -> JobName
hashMsg = JobName . toBytes . hashMsg_

encodeMsg :: Msg -> Workload
encodeMsg = Workload . toStrict . encode
