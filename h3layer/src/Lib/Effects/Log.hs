-- | Logging action for the project. Currently just logs the output to terminal.

module Lib.Effects.Log
       ( mainLogAction

       , runAppLogIO
       , runAppLogIO_
       ) where

import Colog (Message, Msg (..), filterBySeverity, richMessageAction)

import Lib.App (App, AppEnv, AppError, runAppAsIO)


-- | Maing log action for the application. Prints message with some metadata to @stdout@.
mainLogAction :: MonadIO m => Severity -> LogAction m Message
mainLogAction severity =
    filterBySeverity severity msgSeverity richMessageAction

----------------------------------------------------------------------------
-- Application runners with runners
----------------------------------------------------------------------------

-- | Runs application like 'runAppAsIO' but also logs error.
runAppLogIO :: AppEnv -> App a -> IO (Either AppError a)
runAppLogIO env app = do
    appRes <- runAppAsIO env app
    logRes <- whenLeft (Right ()) appRes (logMPErrorIO env)
    pure $ appRes <* logRes

-- | Like 'runAppAsIO' but discards result.
runAppLogIO_ :: AppEnv -> App a -> IO ()
runAppLogIO_ env app = void $ runAppLogIO env app

----------------------------------------------------------------------------
-- Internal utilities
----------------------------------------------------------------------------

logMPErrorIO :: AppEnv -> AppError -> IO (Either AppError ())
logMPErrorIO env err = runAppAsIO env $ log E $ show err
