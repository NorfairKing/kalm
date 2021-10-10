{-# LANGUAGE OverloadedStrings #-}

module Kalm
  ( kalm,
  )
where

import Control.Exception (bracket_)
import Control.Lens
import Control.Monad
import Data.Attoparsec.ByteString (takeByteString)
import Data.MIME as MIME
import Data.RFC5322 as RFC5322
import Kalm.DB
import Network.HaskellNet.IMAP.SSL as IMAP
import System.Environment
import System.Exit

kalm :: IO ()
kalm = do
  [imapServer, imapUsername, imapPassword] <- getArgs
  imapConnection <- IMAP.connectIMAPSSL imapServer
  bracket_ (IMAP.login imapConnection imapUsername imapPassword) (IMAP.logout imapConnection) $ do
    boxes <- IMAP.list imapConnection
    print boxes
    IMAP.select imapConnection "review"
    uids <- IMAP.search imapConnection [ALLs]
    print uids
    forM_ uids $ \uid -> do
      rawContents <- fetch imapConnection uid
      print rawContents

      case RFC5322.parse (RFC5322.message mime) rawContents of
        Left err -> die err
        Right msg -> do
          let isTextPlain = matchContentType "text" (Just "plain") . view contentType
          print $ firstOf (entities . filtered isTextPlain . body) msg
    pure ()
