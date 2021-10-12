{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kalm.Sync.IMAP where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Attoparsec.ByteString (takeByteString)
import Data.MIME as MIME
import Data.RFC5322 as RFC5322
import qualified Data.Text as T
import Database.Persist
import Database.Persist as DB
import Database.Persist.Sqlite
import Kalm.DB
import Kalm.Env
import Kalm.OptParse
import qualified Network.HaskellNet.IMAP as IMAP
import qualified Network.HaskellNet.IMAP.Connection as IMAP
import qualified Network.HaskellNet.IMAP.SSL as IMAP
import System.Environment
import System.Exit
import UnliftIO

connectToServer :: SyncServerSettings -> IO IMAP.IMAPConnection
connectToServer SyncServerSettings {..} =
  if syncServerSettingSSL
    then
      let imapSettings = IMAP.defaultSettingsIMAPSSL {IMAP.sslPort = syncServerSettingPort}
       in IMAP.connectIMAPSSLWithSettings syncServerSettingHost imapSettings
    else IMAP.connectIMAPPort syncServerSettingHost syncServerSettingPort

withLogin :: MonadUnliftIO m => SyncServerSettings -> IMAP.IMAPConnection -> m result -> m result
withLogin SyncServerSettings {..} imapConnection =
  bracket_
    (liftIO $ IMAP.login imapConnection syncServerSettingUsername syncServerSettingPassword)
    (liftIO $ IMAP.logout imapConnection)
