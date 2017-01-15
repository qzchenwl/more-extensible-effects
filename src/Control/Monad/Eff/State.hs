{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Eff.State (
  State,
  get,
  put,
  runState,
  transactionState
) where

import Control.Monad.Eff
import Control.Monad.Eff.Internal
import Data.OpenUnion
import Data.FTCQueue
import Data.Proxy

data State s a where
  Get :: State s s
  Put :: !s -> State s ()

get :: Member (State s) r => Eff r s
get = send Get

put :: Member (State s) r => s -> Eff r ()
put = send . Put

runState :: s -> Eff (State s ': r) a -> Eff r (a, s)
runState s (Pure a) = return (a, s)
runState s (Impure u q) = case decomp u of
  Right Get -> runState s (qApp q s)
  Right (Put s') -> runState s' (qApp q ())
  Left u' -> Impure u' (tsingleton (\x -> runState s (qApp q x)))

-- |
-- An encapsulated State handler, for transactional semantics
-- The global state is updated only if the transactionState finished
-- successfully
transactionState :: forall s r a. Member (State s) r => Proxy s -> Eff r a -> Eff r a
transactionState _ m = do s <- get; loop s m
  where
    loop :: s -> Eff r a -> Eff r a
    loop s (Pure x) = put s >> return x
    loop s (Impure (u::Union r b) q) = case prj u of
      Just (Get :: State s b) -> loop s  (qApp q s)
      Just (Put s')           -> loop s' (qApp q ())
      _                       -> Impure u (qComps q (loop s))

