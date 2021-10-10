module Kalm.Env where

import Control.Monad.Logger
import Control.Monad.Reader
import Database.Persist as DB
import Database.Persist.Sql as DB

data Env = Env {envConnectionPool :: DB.ConnectionPool}

type KalmM a = ReaderT Env (LoggingT IO) a

runKalmM :: Env -> KalmM a -> LoggingT IO a
runKalmM = flip runReaderT

runDB :: SqlPersistT (LoggingT IO) a -> KalmM a
runDB query = do
  logFunc <- askLoggerIO
  pool <- asks envConnectionPool
  liftIO $ runLoggingT (runSqlPool query pool) logFunc
