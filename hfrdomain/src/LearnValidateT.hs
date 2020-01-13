{-# OPTIONS_GHC -foptimal-applicative-do #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}

module LearnValidateT where

import Control.Monad.Validate (runValidate, refute)

main :: IO ()
main = do 
  let _foo = runValidate (refute ["bang"] *> refute ["boom"])
  -- Left ["bang", "boom"]

  let _bar = 
        let getString = refute ["bang"] *> pure "boom"
            useString a = refute [a]
        in runValidate (getString >>= useString)
  -- Left ["bang"]

  let _baz = 
        let getString = refute ["bang"] *> refute ["boom"]
            useString a = refute [a]
        in runValidate (getString >>= useString)
  -- Left ["bang", "boom"]

  {-
  The above works because although the Monad instance for ValidateT fails as soon as the first 
  refute is executed (as it must due to the way the second argument of >>= depends on the 
  result of its first argument), the Applicative instance runs all branches of <*> and combines 
  the errors produced by all of them. When ApplicativeDo is enabled, this can lead to some “magical” 
  looking error reporting where validation automatically continues on each sub-piece of a piece of 
  data until it absolutely cannot proceed any further.
  -}

  print "Done"