{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MTP.MQTT
  ( runMQTT
  ) where

import           Control.Concurrent    (threadDelay)
import           Control.Exception     (SomeException, try)
import           Control.Monad         (forever)
import qualified Data.ByteString.Char8 as B (unpack)
import           Data.ByteString.Lazy  (ByteString)
import           Data.Maybe            (catMaybes)
import           Data.Text             (Text)
import           Network.MQTT.Client
import           Network.MQTT.Topic    (Filter, mkFilter)
import           Network.URI           (URI, uriFragment)
import           System.Entropy        (getEntropy)
import           System.Log.Logger     (errorM)

genHex :: Int -> IO String
genHex n = concatMap w . B.unpack <$> getEntropy n
  where w ch = let s = "0123456789ABCDEF"
                   x = fromEnum ch
               in [s !! div x 16,s !! mod x 16]

messageCallback :: (Topic -> ByteString -> IO ()) -> MQTTClient -> Topic -> ByteString -> [Property] -> IO ()
messageCallback forward _ topic payload _ = forward topic payload

mkSubscribe :: Text -> Maybe (Filter, SubOptions)
mkSubscribe k =
  case mkFilter k of
    Nothing -> Nothing
    Just t  -> Just (t, subOptions)

runMQTT :: URI -> [Text] -> (Topic -> ByteString -> IO ())-> IO ()
runMQTT mqttURI subs forward = do
  clientId <- genHex 20

  let conf = mqttConfig
        { _msgCB = SimpleCallback (messageCallback forward)
        , _protocol = Protocol311
        }

  forever $ do
    r <- try $ do
      client <- connectURI conf mqttURI { uriFragment = '#':clientId }
      subscribed <- subscribe client (catMaybes $ map mkSubscribe subs) []
      errorM "MTP.MQTT" $ "Subscribed: " ++ show subscribed
      waited <- waitForClient client   -- wait for the the client to disconnect
      errorM "MTP.MQTT" $ "Waited: " ++ show waited

    case r of
      Left (e::SomeException) -> errorM "MTP.MQTT" $ "MQTTClient Error: " ++ show e
      Right _                 -> pure ()

    threadDelay 1000000
