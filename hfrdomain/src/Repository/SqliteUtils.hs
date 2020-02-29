{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Repository.SqliteUtils where

import           Control.Monad.Logger (runStdoutLoggingT, LoggingT,
                                       LogLevel(..), filterLogger, MonadLogger)
import           Control.Monad.Reader (runReaderT)
import           Data.Pool
import           Database.Persist.Sqlite (withSqliteConn, runMigration, SqlPersistT, SqlBackend, runSqlPool)
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Polysemy          
import           Polysemy.Input          

import           Model.Schema

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError     = True
logFilter _ LevelWarn      = True
logFilter _ LevelInfo      = True
logFilter _ LevelDebug     = False
logFilter _ (LevelOther _) = False

runSqliteActionMemory :: (MonadUnliftIO m, MonadLogger m) => SqlPersistT m a -> m a
runSqliteActionMemory action = withSqliteConn ":memory:" $ \backend ->
    runReaderT action backend

runSqliteAction :: SqlPersistT (LoggingT IO) a -> IO a
runSqliteAction action = runStdoutLoggingT $ filterLogger logFilter $
    withSqliteConn "/tmp/domain.db" $ \backend ->
        runReaderT action backend

migrateDB :: IO ()
migrateDB = runSqliteAction (runMigration migrateAll)

runDB :: forall b r. Members [Embed IO, Input (Pool SqlBackend)] r => SqlPersistT IO b -> Sem r b
runDB action = embed . runSqlPool action =<< input