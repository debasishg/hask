{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Repository.AccountRepo where 

import qualified Money as Y
import qualified Data.Text as T

import           Data.Pool
import           Data.Time
import           Data.Maybe (fromJust)
import           Data.Function ((&))
import           Control.Monad.Logger (runStdoutLoggingT)
import           Control.Monad.IO.Class
import           Control.Lens
import           Polysemy          
import           Polysemy.Input          
import           Database.Persist (get, insert_, replace, selectList, (==.))
import           Database.Persist.Sqlite (SqlBackend, SqlPersistT, runSqlPool, withSqlitePool)

import           Model.Account
import           Model.Schema

-- | Repository abstraction that's independent of the underlying database 
-- representation
data AccountRepo m a where
    QueryAccount         :: T.Text -> AccountRepo m (Maybe Account)
    Store                :: Account -> AccountRepo m ()
    QueryByOpenDate      :: UTCTime -> AccountRepo m [Account]
    AllAccounts          :: AccountRepo m [Account]
    Upsert               :: Account -> AccountRepo m Account

makeSem ''AccountRepo

runDB :: forall b r. Members [Embed IO, Input (Pool SqlBackend)] r => SqlPersistT IO b -> Sem r b
runDB action = embed . runSqlPool action =<< input

runAccountRepo :: forall r b. Members [Embed IO, Input (Pool SqlBackend)] r => Sem (AccountRepo ': r) b -> Sem r b
runAccountRepo = interpret $ \case
  QueryAccount ano -> runDB (get (AccountKey ano)) 
  Store acc        -> runDB (insert_ acc)
  AllAccounts      -> runDB doAllAccounts
    where 
      doAllAccounts = do
        es <- selectList [] []
        return $ unEntity <$> es
  QueryByOpenDate date -> runDB (doQueryByOpenDate date)
    where
      doQueryByOpenDate dt = do 
        es <- selectList [AccountOpenDate ==. dt] []
        return $ unEntity <$> es
  Upsert acc -> runDB (doUpsert acc)
    where
      doUpsert ac = do 
        a <- get (AccountKey $ ac ^. accountNo)
        _ <- maybe (insert_ ac) (replace (AccountKey $ ac ^. accountNo)) a
        return ac

runAllEffects :: Pool SqlBackend -> Sem '[AccountRepo, Input (Pool SqlBackend), Embed IO] a -> IO a
runAllEffects conn program =
  program                    
    & runAccountRepo
    & runInputConst conn  
    & runM

addAccount :: Pool SqlBackend -> Account -> IO ()
addAccount conn account = 
  runAllEffects conn (store account)

query :: Pool SqlBackend -> T.Text -> IO (Maybe Account)
query conn ano = 
  runAllEffects conn (queryAccount ano)

queryAllAccounts :: Pool SqlBackend -> IO [Account]
queryAllAccounts conn =
  runAllEffects conn allAccounts

queryAccountsByOpenDate :: Pool SqlBackend -> UTCTime -> IO [Account]
queryAccountsByOpenDate conn dt =
  runAllEffects conn (queryByOpenDate dt)

insertOrUpdate :: Pool SqlBackend -> Account -> IO Account
insertOrUpdate conn acc = 
  runAllEffects conn (upsert acc)

connectionString :: T.Text
connectionString = "/tmp/domain.db"

openConnections :: Int
openConnections = 3

main :: Account -> IO ()
main a = runStdoutLoggingT 
             . withSqlitePool connectionString openConnections 
                 $ \pool -> liftIO $ do
                       addAccount pool a 
                       acc <- query pool (a ^. accountNo) 
                       let modified = fromJust acc & currentBalance %~ (+ (100 :: Y.Dense "USD"))
                       updated <- insertOrUpdate pool modified 
                       query pool (updated ^. accountNo) >>= printResult

  where
    printResult (Just ac)  = print ac
    printResult Nothing = putStrLn "Not found"

-- runMigrateActions >> openNewAccounts >>= \accs -> Repository.AccountRepo.main (Prelude.head accs)