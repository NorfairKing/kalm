{-# LANGUAGE RecordWildCards #-}

module Kalm.SyncSpec (spec) where

import Control.Monad
import Control.Monad.Logger
import qualified Data.ByteString as SB
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kalm.DB
import Kalm.Env
import Kalm.OptParse
import Kalm.Sync
import Kalm.Sync.IMAP
import qualified Network.HaskellNet.IMAP as IMAP
import qualified Network.HaskellNet.IMAP.Connection as IMAP
import qualified Network.HaskellNet.IMAP.SSL as IMAP
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
      let syncSettingServers = [localSyncServerSettings]
      let syncSets = SyncSettings {..}
      pure () :: IO ()
      runStderrLoggingT $ runKalmM env $ kalmSync syncSets

dovecotSpec :: SetupFunc Env
dovecotSpec = do
  env <- envSetupFunc
  liftIO $ do
    let server = localSyncServerSettings
    imapConnection <- connectToServer server
    withLogin server imapConnection $ cleanupDovecotServer imapConnection
  pure env

localSyncServerSettings :: SyncServerSettings
localSyncServerSettings =
  SyncServerSettings
    { syncServerSettingHost = "127.0.0.1",
      syncServerSettingPort = 143,
      syncServerSettingUsername = "syd",
      syncServerSettingPassword = "secret",
      syncServerSettingSSL = False
    }

cleanupDovecotServer :: IMAP.IMAPConnection -> IO ()
cleanupDovecotServer imapConnection = do
  boxes <- IMAP.list imapConnection
  forM_ boxes $ \(_, box) -> do
    IMAP.delete imapConnection box
  pure ()

envSetupFunc :: SetupFunc Env
envSetupFunc = do
  envConnectionPool <- connectionPoolSetupFunc automaticMigrations
  pure Env {..}
