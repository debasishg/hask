module Repository.AccountRepository where

import Data.Text
import Data.Time
import Account

class (Monad m) => AccountRepository m where
    query            :: Text                     -- ^ query by account number
                     -> m (Maybe Account)        -- ^ the fetched account, if found
    store            :: Account                  -- ^ store an account
                     -> m Account                
    queryByOpenDate  :: UTCTime                  -- ^ query by account open date
                     -> m [Account]              -- ^ the fetched account list, (maybe empty)
    allAccounts      :: m [Account]              -- ^ fetch all accounts