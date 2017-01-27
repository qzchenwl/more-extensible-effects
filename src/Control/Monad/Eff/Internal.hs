{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Control.Monad.Eff.Internal where

import Data.OpenUnion
import Data.FTCQueue

-- |
-- Effectful arrow type: a function from a to b that also does effects
-- denoted by r
type Arr r a b = a -> Eff r b

-- |
-- An effectful function from 'a' to 'b' that is a composition
-- of several effectful functions. The paremeter r describes the overall
-- effect.
-- The composition members are accumulated in a type-aligned queue
type Arrs r a b = FTCQueue (Eff r) a b

-- |
-- The Eff monad (not a transformer!)
-- It is a fairly standard coroutine monad
-- It is NOT a Free monad! There are no Functor constraints
-- Status of a coroutine (client): done with the value of type w,
-- or sending a request of type Union r with the continuation
-- Arrs r b a.
-- Potentially, inline Union into Impure
data Eff r a where
  Pure :: a -> Eff r a
  Impure :: Union r x -> Arrs r x a -> Eff r a

-- | Application to the `generalized effectful function' Arrs r b w
qApp :: Arrs r b w -> b -> Eff r w
qApp q x =
   case tviewl q of
   TOne k  -> k x
   k :| t -> case k x of
     Pure y -> qApp t y
     Impure u q -> Impure u (q >< t)

-- | Compose effectful arrows (and possibly change the effect!)
qComp :: Arrs r a b -> (Eff r b -> Eff r' c) -> Arr r' a c
qComp g h = \a -> h (qApp g a)

qComps :: Arrs r a b -> (Eff r b -> Eff r' c) -> Arrs r' a c
qComps g h = tsingleton (qComp g h)

instance Functor (Eff r) where
  fmap f (Pure x)     = Pure (f x)
  fmap f (Impure u q) = Impure u (q |> (Pure . f))

instance Applicative (Eff r) where
  pure = Pure
  Pure f <*> Pure x     = Pure (f x)
  Pure f <*> Impure u q = Impure u (q |> (Pure . f))
  Impure u q <*> Pure x = Impure u (q |> (Pure . ($ x)))
  Impure u q <*> m      = Impure u (q |> (`fmap` m))

instance Monad (Eff r) where
  Pure x     >>= k = k x
  Impure u q >>= k = Impure u (q |> k)

-- | send a request and wait for a reply
send :: Member t r => t v -> Eff r v
send t = Impure (inj t) (tsingleton Pure)

run :: Eff '[] w -> w
run (Pure x) = x
run _        = error "run: Impure should never happen"
-- the other case is unreachable since Union [] a cannot be
-- constructed.
-- Therefore, run is a total function if its argument terminates.

-- | Handler type
type Handler t r w = forall v. t v -> Arr r v w -> Eff r w

-- | Parameterized Handler type
type HandlerS s t r w = forall v. s -> t v -> (s -> Arr r v w) -> Eff r w

-- |
-- A convenient pattern: given a request (open union), either
-- handle it or relay it.
handleRelay :: (a -> Eff r w) -> Handler t r w -> Eff (t ': r) a -> Eff r w
handleRelay ret _ (Pure x) = ret x
handleRelay ret h (Impure u q) = case decomp u of
  Right x -> h x k
  Left  u -> Impure u (tsingleton k)
  where k = qComp q (handleRelay ret h)

-- | Parameterized handleRelay
handleRelayS :: s -> (s -> a -> Eff r w) -> HandlerS s t r w -> Eff (t ': r) a -> Eff r w
handleRelayS s ret _ (Pure x) = ret s x
handleRelayS s ret h (Impure u q) = case decomp u of
  Right x -> h s x k
  Left  u -> Impure u (tsingleton (k s))
  where k s x = handleRelayS s ret h (qApp q x)

-- Add something like Control.Exception.catches? It could be useful
-- for control with cut.

-- |
-- Intercept the request and possibly reply to it, but leave it unhandled
-- (that's why we use the same r all throuout)
interpose :: Member t r =>
             (a -> Eff r w) -> (forall v. t v -> Arr r v w -> Eff r w) ->
             Eff r a -> Eff r w
interpose ret h (Pure x) = ret x
interpose ret h (Impure u q) = case prj u of
  Just x -> h x k
  _      -> Impure u (tsingleton k)
  where k = qComp q (interpose ret h)

