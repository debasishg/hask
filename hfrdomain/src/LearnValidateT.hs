{-# OPTIONS_GHC -foptimal-applicative-do #-}
{-# LANGUAGE FlexibleContexts #-}

module LearnValidateT where

import Control.Monad.Validate (runValidateT, runValidate, refute)

main :: IO ()
main = do 
  let foo = runValidate (refute ["bang"] *> refute ["boom"])
  -- Left ["bang", "boom"]

  let bar = 
        let getString = refute ["bang"] *> pure "boom"
            useString a = refute [a]
        in runValidate (getString >>= useString)
  -- Left ["bang"]

  print "Done"