{-# LANGUAGE OverloadedStrings #-}

module Repository.SqliteUtils where

import           Control.Monad.Logger (runStdoutLoggingT, LoggingT,
                                       LogLevel(..), filterLogger)
import           Control.Monad.Reader (runReaderT)

import           Database.Persist.Sqlite (withSqliteConn, runMigration, SqlPersistT)
import           Schema

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError     = True
logFilter _ LevelWarn      = True
logFilter _ LevelInfo      = True
logFilter _ LevelDebug     = False
logFilter _ (LevelOther _) = False

runSqliteAction :: SqlPersistT (LoggingT IO) a -> IO a
runSqliteAction action = runStdoutLoggingT $ filterLogger logFilter $
    withSqliteConn ":memory:" $ \backend ->
        runReaderT action backend

migrateDB :: IO ()
migrateDB = runSqliteAction (runMigration migrateAll)