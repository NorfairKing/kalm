{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Kalm.OptParse where

import Control.Applicative
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml
import qualified Env
import GHC.Generics (Generic)
import Network.Socket
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse (string)
import Path
import Path.IO
import YamlParse.Applicative as YamlParse

data Instructions
  = Instructions !Dispatch !Settings
  deriving (Show, Eq, Generic)

getInstructions :: IO Instructions
getInstructions = do
  args@(Arguments _ flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions args env config

data Settings = Settings
  deriving (Show, Eq, Generic)

data Dispatch
  = DispatchSync !SyncSettings
  deriving (Show, Eq, Generic)

data SyncSettings = SyncSettings
  { syncSettingServers :: ![SyncServerSettings]
  }
  deriving (Show, Eq, Generic)

data SyncServerSettings = SyncServerSettings
  { syncServerSettingHost :: !String,
    syncServerSettingPort :: !PortNumber,
    syncServerSettingUsername :: !String,
    syncServerSettingPassword :: !String,
    syncServerSettingSSL :: !Bool
  }
  deriving (Show, Eq, Generic)

combineToInstructions :: Arguments -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (Arguments cmd Flags {..}) Environment {..} mConf = do
  disp <-
    -- Resolve the command-specific settings for each command
    case cmd of
      CommandSync SyncArgs -> do
        let combineToSyncServerSettings :: SyncServerConfiguration -> SyncServerSettings
            combineToSyncServerSettings SyncServerConfiguration {..} =
              let syncServerSettingHost = syncServerConfHost
                  syncServerSettingSSL = fromMaybe True syncServerConfSSL
                  syncServerSettingPort = fromMaybe (if syncServerSettingSSL then 993 else 143) syncServerConfPort
                  syncServerSettingUsername = syncServerConfUsername
                  syncServerSettingPassword = syncServerConfPassword
               in SyncServerSettings {..}
        let syncSettingServers = map combineToSyncServerSettings $ fromMaybe [] $ syncConfServers <$> mc confSyncConfiguration
        pure $ DispatchSync SyncSettings {..}
  pure $ Instructions disp Settings
  where
    mc :: (Configuration -> Maybe a) -> Maybe a
    mc f = mConf >>= f

data Configuration = Configuration
  { confSyncConfiguration :: !(Maybe SyncConfiguration)
  }
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    objectParser "Configuration" $
      Configuration
        <$> optionalField "sync" "Synchronisation configuration"

data SyncConfiguration = SyncConfiguration
  { syncConfServers :: ![SyncServerConfiguration]
  }
  deriving (Show, Eq, Generic)

instance FromJSON SyncConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema SyncConfiguration where
  yamlSchema =
    objectParser "SyncConfiguration" $
      SyncConfiguration
        <$> optionalFieldWithDefault "servers" [] "The IMAP servers to sync with"

data SyncServerConfiguration = SyncServerConfiguration
  { syncServerConfHost :: !String,
    syncServerConfPort :: !(Maybe PortNumber),
    syncServerConfUsername :: !String,
    syncServerConfPassword :: !String,
    syncServerConfSSL :: !(Maybe Bool)
  }
  deriving (Show, Eq, Generic)

instance FromJSON SyncServerConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema SyncServerConfiguration where
  yamlSchema =
    objectParser "SyncServerConfiguration" $
      SyncServerConfiguration
        <$> requiredField "host" "The IMAP server host; example: imap.fastmail.com"
        <*> optionalField "port" "The port to connect on"
        <*> requiredField "username" "The username of the user to authenticate as"
        <*> requiredField "password" "The password of the user to authenticate as"
        <*> optionalField "ssl" "Connect using ssl"

instance YamlSchema PortNumber where
  yamlSchema = (fromIntegral :: Int -> PortNumber) <$> yamlSchema

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> defaultConfigFile >>= YamlParse.readConfigFile
    Just cf -> do
      afp <- resolveFile' cf
      YamlParse.readConfigFile afp

defaultConfigFile :: IO (Path Abs File)
defaultConfigFile = do
  xdgConfigDir <- getXdgDir XdgConfig (Just [reldir|kalm|])
  resolveFile xdgConfigDir "config.yaml"

data Environment = Environment
  { envConfigFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq, Generic)

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "KALM_" $
    Environment
      <$> Env.var (fmap Just . Env.str) "CONFIG_FILE" (Env.def Nothing <> Env.keep <> Env.help "Config file")

data Arguments
  = Arguments !Command !Flags
  deriving (Show, Eq, Generic)

getArguments :: IO Arguments
getArguments = customExecParser prefs_ argParser

prefs_ :: OptParse.ParserPrefs
prefs_ =
  OptParse.defaultPrefs
    { OptParse.prefShowHelpOnError = True,
      OptParse.prefShowHelpOnEmpty = True
    }

argParser :: OptParse.ParserInfo Arguments
argParser =
  OptParse.info
    (OptParse.helper <*> parseArgs)
    (OptParse.fullDesc <> OptParse.footerDoc (Just $ OptParse.string footerStr))
  where
    footerStr =
      unlines
        [ Env.helpDoc environmentParser,
          "",
          "Configuration file format:",
          T.unpack (YamlParse.prettyColourisedSchemaDoc @Configuration)
        ]

parseArgs :: OptParse.Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

data Command
  = CommandSync !SyncArgs
  deriving (Show, Eq, Generic)

parseCommand :: OptParse.Parser Command
parseCommand =
  OptParse.hsubparser $
    mconcat
      [ OptParse.command "sync" $ CommandSync <$> parseCommandSync
      ]

data SyncArgs = SyncArgs
  deriving (Show, Eq, Generic)

parseCommandSync :: OptParse.ParserInfo SyncArgs
parseCommandSync = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Do an IMAP sync"
    parser = pure SyncArgs

data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq, Generic)

parseFlags :: OptParse.Parser Flags
parseFlags =
  Flags
    <$> optional
      ( strOption
          ( mconcat
              [ long "config-file",
                help "Give the path to an altenative config file",
                metavar "FILEPATH"
              ]
          )
      )
