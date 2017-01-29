{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Eff.Writer (
  Writer(..),
  tell, writer,
  runWriter
) where

import Control.Monad.Eff
import Data.Monoid

data Writer o a where
  Put :: o -> Writer o ()

tell :: Member (Writer o) r => o -> Eff r ()
tell o = send (Put o)

writer :: Member (Writer o) r => (a, o) -> Eff r a
writer (a, o) = do
  tell o
  return a

runWriter :: Monoid o => Eff (Writer o ': r) a -> Eff r (a, o)
runWriter = handleRelay ret handle
  where
    ret :: Monoid o => a -> Eff r (a, o)
    ret a = return (a, mempty)

    handle :: Monoid o => Handler (Writer o) r (a, o)
    handle (Put o) k = do
      (a, os) <- k ()
      return (a, o <> os)

