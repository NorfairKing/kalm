{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kalm.Sync (kalmSync) where

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
import Kalm.Sync.IMAP
import qualified Network.HaskellNet.IMAP as IMAP
import qualified Network.HaskellNet.IMAP.Connection as IMAP
import qualified Network.HaskellNet.IMAP.SSL as IMAP
import System.Environment
import System.Exit
import UnliftIO

kalmSync :: SyncSettings -> KalmM ()
kalmSync SyncSettings {..} =
  forM_ syncSettingServers $ \serverSets@SyncServerSettings {..} -> do
    imapConnection <- liftIO $ connectToServer serverSets

    logDebugN $ T.pack $ "Connected to " <> syncServerSettingHost

    capabilities <- liftIO $ IMAP.capability imapConnection
    liftIO $ print capabilities

    withLogin serverSets imapConnection $ do
      boxes <- liftIO $ IMAP.list imapConnection
      liftIO $ print boxes

      let interestingBoxes = ["INBOX", "Drafts", "Sent", "rd", "ref", "reply", "review", "wait"]
      forM_ boxes $ \(_, mailboxName) -> when (mailboxName `elem` interestingBoxes) $
        bracket_ (liftIO $ IMAP.select imapConnection mailboxName) (liftIO $ IMAP.close imapConnection) $ do
          liftIO $ print mailboxName
          status <-
            liftIO $
              IMAP.status
                imapConnection
                mailboxName
                [ IMAP.MESSAGES,
                  IMAP.RECENT,
                  IMAP.UIDNEXT,
                  IMAP.UIDVALIDITY,
                  IMAP.UNSEEN
                ]
          liftIO $ print status
          uids <- liftIO $ IMAP.search imapConnection [IMAP.ALLs]
          liftIO $ print uids
          forM_ uids $ \uid -> do
            liftIO $ print uid
            size <- liftIO $ IMAP.fetchSize imapConnection uid
            liftIO $ print size
            contents <- liftIO $ IMAP.fetch imapConnection uid
            runDB $ DB.insert_ (Email {emailMailboxName = mailboxName, emailContents = contents})
