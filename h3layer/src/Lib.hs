module Lib
       ( mkAppEnv
       , runServer
       , main
       ) where

import Lib.App (AppEnv, Env (..))
import Lib.Config (Config (..), loadConfig)
import Lib.Db (initialisePool)
import Lib.Effects.Log (mainLogAction)

-- import qualified Data.HashMap.Strict as HashMap

mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config{..} = do
    -- IO configuration
    envDbPool   <- initialisePool cDbCredentials

    -- pure configuration
    let envLogAction = mainLogAction cLogSeverity
    pure Env{..}

runServer :: AppEnv -> IO ()
runServer env@Env{..} = do
    pure ()

main :: IO ()
main = loadConfig >>= mkAppEnv >>= runServer