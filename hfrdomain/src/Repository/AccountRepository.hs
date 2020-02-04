{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Repository.AccountRepository (AccountRepository (..)) where 

import Data.Text
import Data.Time
import Model.Account

-- | Repository abstraction that's independent of the underlying database 
-- representation
class (Monad m) => AccountRepository m where
    query            :: Text                     -- ^ query by account number
                     -> m (Maybe Account)        -- ^ the fetched account, if found
    store            :: Account                  -- ^ store an account
                     -> m Account                
    queryByOpenDate  :: UTCTime                  -- ^ query by account open date
                     -> m [Account]              -- ^ the fetched account list, (maybe empty)
    allAccounts      :: m [Account]              -- ^ fetch all accounts
    upsert           :: Account                  -- ^ store an account
                     -> m Account                
