{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib
  ( someFunc
  ) where

import           Data.String         (fromString)
import           Data.Text           (Text)
import           Metro.Utils         (setupLog)
import           MTP.MQTT            (runMQTT)
import           MTP.Types
import           Network.MQTT.Topic  (unTopic)
import           Network.URI         (parseURI)
import           Options.Applicative
import           Periodic.ClientPool (ClientPoolEnv, openPool, runClientPoolM,
                                      submitJob)
import           Periodic.Types      (FuncName)
import           System.Log.Logger   (Priority (INFO), errorM, infoM)

data Options = Options
    { funcName :: String
    , host     :: String
    , mqtt     :: String
    , poolSize :: Int
    , subList  :: [Text]
    }
    deriving (Show)

parser :: Parser Options
parser = Options
  <$> strOption (long "func"
                 <> short 'f'
                 <> metavar "FUNC"
                 <> help "Periodic funcname."
                 <> value "mqtt-bridge")
  <*> strOption (long "host"
                 <> short 'h'
                 <> metavar "HOST"
                 <> help "Periodic server host."
                 <> value "unix:///tmp/periodicd.sock")
  <*> strOption (long "mqtt-uri"
                 <> metavar "MQTTURI"
                 <> help "mqtt server."
                 <> value "mqtt://localhost:1883")
  <*> option auto (long "pool-size"
                 <> metavar "POOLSIZE"
                 <> help "Resource pool size."
                 <> value 100)
  <*> some (strArgument (help "subscribe topics"))

someFunc :: IO ()
someFunc = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      ( fullDesc
     <> progDesc "Mqtt to periodic bridge"
     <> header "mqtt-to-periodic - Mqtt to periodic bridge" )

program :: Options -> IO ()
program Options {..} = do
  setupLog INFO
  case parseURI mqtt of
    Nothing -> errorM "Lib" "Invalid mqtt uri"
    Just mqttURI -> do
      env <- openPool host poolSize
      runMQTT mqttURI subList $ \sub payload -> do
        result <- processMsg env (fromString funcName) (msg sub payload)
        infoM "Lib" $ "topic=" ++ show (unTopic sub) ++ ", result=" ++ show result

processMsg :: ClientPoolEnv -> FuncName -> Msg -> IO Bool
processMsg env func m =
  runClientPoolM env $ submitJob func (hashMsg m) (encodeMsg m) 0 0
