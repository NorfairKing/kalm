{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Kalm.DB where

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
import Kalm.DB.Message

-- When adding a table here, be sure to add the corresponding roundtrip test as well.
share
  [mkPersist sqlSettings, mkMigrate "automaticMigrations"]
  [persistLowerCase|

Email
  mailbox Text
  contents MIMEMessage

  deriving Show
  deriving Eq
  deriving Generic

|]

instance Validity Email
