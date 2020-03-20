{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Repository.AccountCache where

import qualified Data.Text as T
import           Control.Monad (void)
import           Data.ByteString.Char8 (pack, unpack)
import           Polysemy          
import           Polysemy.Input
import           Database.Redis (Redis, get, setex, mset, del, runRedis, Connection)
import           Control.Lens

import           Model.Account

data AccountCache m a where
    CacheAccount         :: Account -> AccountCache m ()
    CacheAccounts        :: [Account] -> AccountCache m ()
    FetchCachedAccount   :: T.Text -> AccountCache m (Maybe Account)
    DeleteCachedAccount  :: T.Text -> AccountCache m ()

makeSem ''AccountCache

runCache :: forall b r. Members [Embed IO, Input Connection] r => Redis b -> Sem r b
runCache action = embed . flip runRedis action =<< input 

runAccountCache :: forall r b. Members [Embed IO, Input Connection] r => Sem (AccountCache ': r) b -> Sem r b
runAccountCache = interpret $ \case
  CacheAccount acc -> runCache (void $ setex (pack . show $ acc ^. accountNo) 3600 (pack . show $ acc))
  CacheAccounts accs -> runCache (void $ mset $ fmap (\acc -> ((pack . show $ acc ^. accountNo), (pack . show $ acc))) accs)

  FetchCachedAccount ano -> runCache doFetch
    where
        doFetch = do
          result <- get (pack . show $ ano)
          case result of
            Right (Just accountString) -> return $ Just (read . unpack $ accountString)
            _ -> return Nothing

  DeleteCachedAccount ano -> runCache (void $ del [pack . show $ ano])