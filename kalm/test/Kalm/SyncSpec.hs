{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kalm.SyncSpec (spec) where

import Control.Monad
import Control.Monad.Logger
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.CaseInsensitive as CI
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Text ()
import Data.MIME
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist (Entity (..))
import qualified Database.Persist as DB
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
import Test.QuickCheck
import Test.Syd
import Test.Syd.Path
import Test.Syd.Persistent.Sqlite
import Test.Syd.Process.Typed
import Test.Syd.Validity

instance (GenValid a, FoldCase a) => GenValid (CI a) where
  genValid = CI.mk <$> genValid
  shrinkValid = fmap CI.mk . shrinkValid . CI.original

instance GenValid Headers where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid MIME where
  genValid = sized $ \n ->
    oneof
      [ Part <$> genValid,
        Encapsulated <$> genValid,
        Multipart <$> genValid
        -- TODO FailedParse as well
      ]
  shrinkValid = \case
    Part bs -> Part <$> shrinkValid bs
    Encapsulated p -> Encapsulated <$> shrinkValid p
    Multipart ne -> Multipart <$> shrinkValid ne
    FailedParse _ _ -> []

instance GenValid (Message EncStateWire MIME) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Email where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

spec :: Spec
spec = dovecotSpec $ do
  let localSyncSets =
        let syncSettingServers = [localSyncServerSettings]
         in SyncSettings {..}
  xit "can store a message on a server" $ \env ->
    forAllValid $ \email -> do
      pure () :: IO ()
      runStderrLoggingT $
        runKalmM env $ do
          -- Insert an email
          emailId <- runDB $ DB.insert (email :: Email)
          -- Do the sync
          kalmSync localSyncSets
          -- Check that the email is still there
          mEmailAfter <- runDB $ DB.get emailId

          liftIO $ case mEmailAfter of
            Nothing -> expectationFailure "Should have found the email."
            Just emailAfter -> emailContents emailAfter `shouldSatisfy` (`eqMessage` emailContents email)

  pending "deletes an email locally once it's gone from the server"
  pending "deletes an email remotely once it's gone locally"

  xit "can retrieve a message from a server" $ \env ->
    forAllValid $ \emailPrototype -> do
      let email = emailPrototype {emailMailbox = "TEST"} -- TODO don't do this with a dummy
      pure () :: IO ()
      -- Insert an email on the server
      withTestConnection $ \imapConnection -> do
        IMAP.append imapConnection (T.unpack (emailMailbox email)) (LB.toStrict (renderMessage (emailContents email)))

      runStderrLoggingT $
        runKalmM env $ do
          -- Do the sync
          kalmSync localSyncSets
          -- Check that the email is still there
          mEmailAfter <- runDB $ DB.selectFirst [] []
          liftIO $ case mEmailAfter of
            Nothing -> expectationFailure "Expected to have found an email."
            Just (Entity _ emailAfter) -> emailAfter `shouldBe` email

dovecotSpec :: SpecWith Env -> Spec
dovecotSpec =
  modifyMaxSuccess (`div` 10)
    . doNotRandomiseExecutionOrder
    . sequential
    . setupAround dovecotSetupFunc

dovecotSetupFunc :: SetupFunc Env
dovecotSetupFunc = do
  env <- envSetupFunc
  liftIO $ do
    let server = localSyncServerSettings
    withTestConnection cleanupDovecotServer
  pure env

withTestConnection :: (IMAP.IMAPConnection -> IO result) -> IO result
withTestConnection func = do
  imapConnection <- connectToServer localSyncServerSettings
  withLogin localSyncServerSettings imapConnection $ func imapConnection

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
  forM_ boxes $ \(_, box) -> unless (box == "INBOX") $ do
    IMAP.delete imapConnection box
  pure ()

envSetupFunc :: SetupFunc Env
envSetupFunc = do
  envConnectionPool <- connectionPoolSetupFunc automaticMigrations
  pure Env {..}
