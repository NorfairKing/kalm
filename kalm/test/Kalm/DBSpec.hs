{-# LANGUAGE TypeApplications #-}

module Kalm.DBSpec (spec) where

import Data.MIME
import Kalm.DB.Gen
import Kalm.DB.TestUtils
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Persist

spec :: Spec
spec = do
  persistSpecOnValid @MIMEMessage
