{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kalm
  ( kalm,
  )
where

import Control.Exception (bracket_)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Attoparsec.ByteString (takeByteString)
import Data.MIME as MIME
import Data.RFC5322 as RFC5322
import Database.Persist
import Database.Persist as DB
import Database.Persist.Sqlite
import Kalm.DB
import Kalm.OptParse
import qualified Network.HaskellNet.IMAP.SSL as IMAP
import System.Environment
import System.Exit

kalm :: IO ()
kalm = do
  Instructions (DispatchSync SyncSettings {..}) Settings <- getInstructions
  runStderrLoggingT $
    withSqlitePool "kalm.sqlite3" 1 $ \connectionPool -> do
      runSqlPool (runMigration automaticMigrations) connectionPool
      liftIO $
        forM_ syncSettingServers $ \SyncServerSettings {..} -> do
          imapConnection <- IMAP.connectIMAPSSL syncServerSettingHost

          capabilities <- IMAP.capability imapConnection
          print capabilities

          bracket_ (IMAP.login imapConnection syncServerSettingUsername syncServerSettingPassword) (IMAP.logout imapConnection) $ do
            boxes <- IMAP.list imapConnection
            print boxes

            let interestingBoxes = ["INBOX", "Drafts", "Sent", "rd", "ref", "reply", "review", "wait"]
            forM_ boxes $ \(_, mailboxName) -> when (mailboxName `elem` interestingBoxes) $
              bracket_ (IMAP.select imapConnection mailboxName) (IMAP.close imapConnection) $ do
                print mailboxName
                status <-
                  IMAP.status
                    imapConnection
                    mailboxName
                    [ IMAP.MESSAGES,
                      IMAP.RECENT,
                      IMAP.UIDNEXT,
                      IMAP.UIDVALIDITY,
                      IMAP.UNSEEN
                    ]
                print status
                uids <- IMAP.search imapConnection [IMAP.ALLs]
                print uids
                forM_ uids $ \uid -> do
                  print uid
                  size <- IMAP.fetchSize imapConnection uid
                  print size
                  contents <- IMAP.fetch imapConnection uid
                  runSqlPool (DB.insert (Email {emailMailboxName = mailboxName, emailContents = contents})) connectionPool
