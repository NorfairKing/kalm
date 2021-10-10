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
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics (Generic)

-- When adding a table here, be sure to add the corresponding roundtrip test as well.
share
  [mkPersist sqlSettings, mkMigrate "automaticMigrations"]
  [persistLowerCase|

Email
  body ByteString

  deriving Show
  deriving Eq
  deriving Generic

|]
