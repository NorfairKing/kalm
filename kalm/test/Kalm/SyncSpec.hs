{-# LANGUAGE RecordWildCards #-}

module Kalm.SyncSpec (spec) where

import Control.Monad.Logger
import qualified Data.ByteString as SB
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kalm.DB
import Kalm.Env
import Kalm.OptParse
import Kalm.Sync
import Network.Socket
import Network.Socket.Free
import Network.Socket.Wait as Port
import Path
import Path.IO
import System.Process.Typed
import Test.Syd
import Test.Syd.Path
import Test.Syd.Persistent.Sqlite
import Test.Syd.Process.Typed

spec :: Spec
spec = do
  setupAround envSetupFunc $
    it "local" $ \env -> do
      let syncSettingServers =
            [ SyncServerSettings
                { syncServerSettingHost = "127.0.0.1",
                  syncServerSettingPort = 143,
                  syncServerSettingUsername = "syd",
                  syncServerSettingPassword = "secret",
                  syncServerSettingSSL = False
                }
            ]
      let syncSets = SyncSettings {..}
      pure () :: IO ()
      runStderrLoggingT $ runKalmM env $ kalmSync syncSets

envSetupFunc :: SetupFunc Env
envSetupFunc = do
  envConnectionPool <- connectionPoolSetupFunc automaticMigrations
  pure Env {..}
