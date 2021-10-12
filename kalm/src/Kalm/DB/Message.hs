{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kalm.DB.Message where

import Control.Arrow (left)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.CaseInsensitive
import Data.MIME
import Data.Proxy
import Data.RFC5322 as RFC5322
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Text ()
import Data.Word
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics (Generic)
import Kalm.DB.Mailbox

instance Validity a => Validity (CI a) where
  validate ci =
    mconcat
      [ delve "original" (original ci),
        delve "foldedCase" (foldedCase ci)
      ]

instance Validity Headers

instance Validity MIME where
  validate = \case
    Part sb -> validate sb
    Encapsulated mm -> validate mm
    Multipart ne -> validate ne
    FailedParse _ bs -> validate bs

instance Validity a => Validity (Message s a)

instance PersistField (Message EncStateWire MIME) where
  toPersistValue = toPersistValue . LB.toStrict . renderMessage
  fromPersistValue pv = do
    sb <- fromPersistValue pv
    left T.pack $ RFC5322.parse (message mime) (sb :: ByteString)

instance PersistFieldSql (Message EncStateWire MIME) where
  sqlType Proxy = sqlType (Proxy :: Proxy ByteString)
