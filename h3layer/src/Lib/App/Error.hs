{-# LANGUAGE DeriveAnyClass #-}

module Lib.App.Error
       ( AppError (..)
       , AppErrorType
       , AppException (..)
       , WithError
       , throwError

         -- * Error checks
       , isServerError
       , isNotAllowed
       , isInvalid

         -- * Internal error helpers
       , notFound
       , serverError
       , notAllowed
       , invalid
       , dbError
       , dbNamedError
       , limitError

         -- * Error throwing helpers
       , throwOnNothing
       , throwOnNothingM
       , notFoundOnNothing
       , notFoundOnNothingM
       ) where

import Control.Monad.Except (MonadError)
import GHC.Stack (SrcLoc (SrcLoc, srcLocModule, srcLocStartLine))
import PgNamed (PgNamedError)

import qualified Control.Monad.Except as E (throwError)

-- | Type alias for errors.
type WithError m = (MonadError AppError m, HasCallStack)

-- | Specialized version of 'E.throwError'
throwError :: WithError m => AppErrorType -> m a
throwError = E.throwError . AppError (toSourcePosition callStack)
{-# INLINE throwError #-}

newtype SourcePosition = SourcePosition Text
    deriving newtype (Show, Eq)

-- | Display 'CallStack' as 'SourcePosition' in a format: @Module.function#line_number@.
toSourcePosition :: CallStack -> SourcePosition
toSourcePosition cs = SourcePosition showCallStack
  where
    showCallStack :: Text
    showCallStack = case getCallStack cs of
        []                             -> "<unknown loc>"
        [(name, loc)]                  -> showLoc name loc
        (_, loc) : (callerName, _) : _ -> showLoc callerName loc

    showLoc :: String -> SrcLoc -> Text
    showLoc name SrcLoc{..} =
        toText srcLocModule <> "." <> toText name <> "#" <> show srcLocStartLine


-- | 'AppErrorType' with the corresponding 'CallStack'.
data AppError = AppError
    { appErrorCallStack :: !SourcePosition
    , appErrorType      :: !AppErrorType
    } deriving (Show, Eq)

-- | App errors type.
newtype AppErrorType = InternalError IError
    deriving (Show, Eq)

{- | Exception wrapper around 'AppError'. Useful when you need to throw/catch
'AppError' as 'Exception'.
-}
newtype AppException = AppException
    { unAppException :: AppError
    } deriving (Show)
      deriving anyclass (Exception)

{- | The internal errors that can be thrown. These errors are meant to be
handled within the application and cover exceptional circumstances/coding errors.
-}
data IError
    {- | General not found. -}
    = NotFound
    {- | Some exceptional circumstance has happened stop execution and return.
    Optional text to provide some context in server logs.
    -}
    | ServerError Text
    {- | A required permission level was not met. Optional text to provide some context. -}
    | NotAllowed Text
    {- | Given inputs do not conform to the expected format or shape. Optional
    text to provide some context in server logs.
    -}
    | Invalid Text
    -- | Data base specific errors.
    | DbError Text
    -- | Data base named parameters errors.
    | DbNamedError PgNamedError
    -- | Limits on the multi-request are overflowed.
    | LimitError
    deriving (Show, Eq)

----------------------------------------------------------------------------
-- Error checks
----------------------------------------------------------------------------

isServerError :: AppErrorType -> Bool
isServerError (InternalError (ServerError _)) = True
isServerError _                               = False

isNotAllowed :: AppErrorType -> Bool
isNotAllowed (InternalError (NotAllowed _)) = True
isNotAllowed _                              = False

isInvalid :: AppErrorType -> Bool
isInvalid (InternalError (Invalid _)) = True
isInvalid _                           = False

----------------------------------------------------------------------------
-- Internal Error helpers
----------------------------------------------------------------------------

notFound :: AppErrorType
notFound = InternalError NotFound

serverError :: Text -> AppErrorType
serverError = InternalError . ServerError

notAllowed :: Text -> AppErrorType
notAllowed = InternalError . NotAllowed

invalid :: Text -> AppErrorType
invalid = InternalError . Invalid

dbError :: Text -> AppErrorType
dbError = InternalError . DbError

dbNamedError :: PgNamedError -> AppErrorType
dbNamedError = InternalError . DbNamedError

limitError :: AppErrorType
limitError = InternalError LimitError

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Extract the value from a maybe, throwing the given 'AppError' if
-- the value does not exist
throwOnNothing :: WithError m => AppErrorType -> Maybe a -> m a
throwOnNothing err = withFrozenCallStack . maybe (throwError err) pure

-- | Extract the value from a 'Maybe' in @m@, throwing the given 'AppError' if
-- the value does not exist
throwOnNothingM :: WithError m => AppErrorType -> m (Maybe a) -> m a
throwOnNothingM err action = withFrozenCallStack $ action >>= throwOnNothing err

-- | Similar to 'throwOnNothing' but throws a 'NotFound' if the value does not exist
notFoundOnNothing :: WithError m => Maybe a -> m a
notFoundOnNothing = withFrozenCallStack . throwOnNothing notFound

-- | Similar to 'throwOnNothingM' but throws a 'NotFound' if the value does not exist
notFoundOnNothingM :: WithError m => m (Maybe a) -> m a
notFoundOnNothingM = withFrozenCallStack . throwOnNothingM notFound
