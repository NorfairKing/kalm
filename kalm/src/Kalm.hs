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
import Kalm.Env
import Kalm.OptParse
import Kalm.Sync
import qualified Network.HaskellNet.IMAP.SSL as IMAP
import System.Environment
import System.Exit

kalm :: IO ()
kalm = do
  Instructions (DispatchSync syncSettings) Settings <- getInstructions
  runStderrLoggingT $
    withSqlitePool "kalm.sqlite3" 1 $ \envConnectionPool -> do
      runSqlPool (runMigration automaticMigrations) envConnectionPool
      let env = Env {..}
      runKalmM env $ kalmSync syncSettings
