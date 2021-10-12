{-# OPTIONS_GHC -Wno-orphans #-}

module Kalm.DB.Mailbox where

import Control.Arrow (left)
import Data.ByteString (ByteString)
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

instance Validity Domain

instance Validity AddrSpec

instance Validity Mailbox

instance PersistField Mailbox where
  toPersistValue = toPersistValue . renderMailbox
  fromPersistValue pv = do
    sb <- fromPersistValue pv
    left T.pack $ RFC5322.parse (RFC5322.mailbox defaultCharsets) (sb :: ByteString)

instance PersistFieldSql Mailbox where
  sqlType Proxy = sqlType (Proxy :: Proxy ByteString)
