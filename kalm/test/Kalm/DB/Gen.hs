{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kalm.DB.Gen where

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
  genValid = sized $ \case
    0 -> Part <$> genValid
    n ->
      oneof
        [ Part <$> genValid,
          -- Encapsulated <$> resize (n - 1) genValid,
          Multipart <$> resize (n - 1) genValid
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
