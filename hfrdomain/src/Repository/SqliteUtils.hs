{-# LANGUAGE OverloadedStrings #-}

module Repository.SqliteUtils where

import           Control.Monad.Logger (runStdoutLoggingT, LoggingT,
                                       LogLevel(..), filterLogger, MonadLogger)
import           Control.Monad.Reader (runReaderT)

import           Database.Persist.Sqlite (withSqliteConn, runMigration, SqlPersistT)
import           Model.Schema
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Text


logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError     = True
logFilter _ LevelWarn      = True
logFilter _ LevelInfo      = True
logFilter _ LevelDebug     = False
logFilter _ (LevelOther _) = False

runSqliteAction2 :: (MonadUnliftIO m, MonadLogger m) => SqlPersistT m a -> m a
runSqliteAction2 action = withSqliteConn ":memory:" $ \backend ->
    runReaderT action backend

runSqliteAction :: SqlPersistT (LoggingT IO) a -> IO a
runSqliteAction action = runStdoutLoggingT $ filterLogger logFilter $
    withSqliteConn "/tmp/domain.db" $ \backend ->
        runReaderT action backend

runSqliteAction1 :: Text -> SqlPersistT (LoggingT IO) a -> IO a
runSqliteAction1 dbfile action = runStdoutLoggingT $ filterLogger logFilter $
    withSqliteConn dbfile $ \backend ->
        runReaderT action backend

migrateDB :: IO ()
migrateDB = runSqliteAction (runMigration migrateAll)