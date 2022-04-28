{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib
  ( someFunc
  ) where

import           Control.Monad       (void)
import           Data.String         (fromString)
import           Data.Text           (Text)
import           Metro.Utils         (setupLog)
import           MTP.MQTT            (runMQTT)
import           MTP.Types
import           Network.URI         (parseURI)
import           Options.Applicative
import           Periodic.ClientPool (ClientPoolEnv, openPool, runClientPoolM,
                                      submitJob)
import           Periodic.Types      (FuncName)
import           System.Log.Logger   (Priority (INFO), errorM)

data Options = Options
    { funcName :: String
    , host     :: String
    , mqtt     :: String
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
      env <- openPool host 100
      runMQTT mqttURI subList $ \sub ->
        void . processMsg env (fromString funcName) . msg sub

processMsg :: ClientPoolEnv -> FuncName -> Msg -> IO Bool
processMsg env func m =
  runClientPoolM env $ submitJob func (hashMsg m) (Just $ encodeMsg m) Nothing
