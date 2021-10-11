module Kalm.Env where

import Control.Monad.Logger
import Control.Monad.Reader
import Database.Persist as DB
import Database.Persist.Sql as DB

data Env = Env {envConnectionPool :: DB.ConnectionPool}

type KalmM a = ReaderT Env (LoggingT IO) a

runKalmM :: MonadLoggerIO m => Env -> KalmM a -> m a
runKalmM env func = do
  logFunc <- askLoggerIO
  liftIO $ runLoggingT (runReaderT func env) logFunc

runDB :: SqlPersistT (LoggingT IO) a -> KalmM a
runDB query = do
  logFunc <- askLoggerIO
  pool <- asks envConnectionPool
  liftIO $ runLoggingT (runSqlPool query pool) logFunc
