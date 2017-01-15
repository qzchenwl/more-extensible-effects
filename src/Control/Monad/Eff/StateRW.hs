{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Control.Monad.Eff.StateRW (
  runStateRW,
  Reader,
  Writer,
  tell,
  ask
) where

import Control.Monad.Eff
import Control.Monad.Eff.Internal
import Control.Monad.Eff.Reader
import Control.Monad.Eff.Writer
import Data.OpenUnion
import Data.FTCQueue

runStateRW :: s -> Eff (Writer s ': Reader s ': r) a -> Eff r (a, s)
runStateRW s (Pure a) = return (a, s)
runStateRW s (Impure u q) = case decomp u of
  Right (Put s') -> k s' ()
  Left u'        -> case decomp u' of
    Right Get -> k s s
    Left u''  -> Impure u'' (tsingleton (k s))
  where k s = qComp q (runStateRW s)
