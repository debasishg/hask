module Combinators where

import qualified Control.Category as C
import Control.Monad.Reader ( (>=>) )

-- | This is a generic pattern of processing a list of monadic functions through
-- `(>>=)`. `composeParts` just composes the Kleisli arrows through a `foldr`. 
-- And `foldBinds` binds them through. Adopted from the SoF thread https://stackoverflow.com/a/8717016
composeParts :: (Monad m) => [a -> m a] -> a -> m a
composeParts = foldr (>=>) return 

foldBinds :: (Monad m) => m a -> [a -> m a] -> m a
foldBinds m fs = m >>= composeParts fs

-- | Programming at a higher level of abstraction. Both Kleisli and function composition
-- are instances of Category - hence can be generalized with such a combinator.
flattenAndCompose :: (C.Category cat) => [cat a a] -> cat a a
flattenAndCompose = foldr (C.>>>) C.id 
