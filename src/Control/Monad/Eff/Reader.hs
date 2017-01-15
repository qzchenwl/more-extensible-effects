{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Eff.Reader (
  Reader(..),
  ask, asks, reader, local,
  runReader
) where

import Control.Monad.Eff

data Reader e a where
  Get :: Reader e e

ask :: Member (Reader e) r => Eff r e
ask = send Get

asks, reader :: Member (Reader e) r => (e -> a) -> Eff r a
asks f = fmap f ask
reader = asks

runReader :: e -> Eff (Reader e ': r) a -> Eff r a
runReader e = handleRelay ret (handle e)

local :: forall e r a. Member (Reader e) r => (e -> e) -> Eff r a -> Eff r a
local f m = do
  e <- ask
  interpose ret (handle (f e)) m

ret :: a -> Eff r a
ret = return

handle :: e -> Handler (Reader e) r a
handle e Get k = k e
