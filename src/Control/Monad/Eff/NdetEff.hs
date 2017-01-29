{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

-- The following is needed to define MonadPlus instance. It is decidable
-- (there is no recursion!), but GHC cannot see that.
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Eff.NdetEff (
  NdetEff,
  makeChoiceA,
  msplit,
  unmsplit,
  ifte,
  once
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Internal
import Data.OpenUnion
import Data.FTCQueue

data NdetEff a where
  MZero :: NdetEff a
  MPlus :: NdetEff Bool

instance Member NdetEff r => Alternative (Eff r) where
  empty = mzero
  (<|>) = mplus

instance Member NdetEff r => MonadPlus (Eff r) where
  mzero = send MZero
  mplus m1 m2 = send MPlus >>= \x -> if x then m1 else m2

-- | An interpreter
-- The following is very simple, but leaks a lot of memory
-- The cause probably is mapping every failure to empty
-- It takes then a lot of timne and space to store those empty
makeChoiceA :: Alternative f => Eff (NdetEff ': r) a -> Eff r (f a)
makeChoiceA = handleRelay ret handle
  where
    ret :: Alternative f => a -> Eff r (f a)
    ret = return . pure

    handle :: Alternative f => Handler NdetEff r (f a)
    handle MZero k = return empty
    handle MPlus k = liftM2 (<|>) (k True) (k False)

-- |
-- A different implementation, more involved but faster and taking
-- much less (100 times) less memory.
-- The benefit of the effect framework is that we can have many
-- interpreters.
makeChoiceA' :: Alternative f => Eff (NdetEff ': r) a -> Eff r (f a)
makeChoiceA' = loop []
  where
    loop []    (Pure x)     = return (pure x)
    loop (h:t) (Pure x)     = loop t h >>= \r -> return (pure x <|> r)
    loop jq    (Impure u q) = case decomp u of
      Right MZero -> case jq of
        []    -> return empty
        (h:t) -> loop t h
      Right MPlus -> loop (qApp q False : jq) (qApp q True)
      Left  u     -> Impure u (tsingleton (loop jq . qApp q ))


-- ------------------------------------------------------------------------
-- Soft-cut: non-deterministic if-then-else, aka Prolog's *->
-- Declaratively,
--    ifte t th el = (t >>= th) `mplus` ((not t) >> el)
-- However, t is evaluated only once. In other words, ifte t th el
-- is equivalent to t >>= th if t has at least one solution.
-- If t fails, ifte t th el is the same as el.

-- We actually implement LogicT, the non-determinism reflection,
-- of which soft-cut is one instance.
-- See the LogicT paper for an explanation
msplit :: Member NdetEff r => Eff r a -> Eff r (Maybe (a, Eff r a))
msplit = loop []
  where
  -- single result
  loop [] (Pure x) = return (Just (x,mzero))
  -- definite result and perhaps some others
  loop jq (Pure x) = return (Just (x, msum jq))
  -- not yet definite answer
  loop jq (Impure u q) = case prj u of
    Just MZero -> case jq of
      -- no futher choices
      []     -> return Nothing
      -- other choices remain, try them
      (j:jq) -> loop jq j
    Just MPlus -> loop (qApp q False : jq) (qApp q True)
    _          -> Impure u (qComps q (loop jq))

-- | Other committed choice primitives can be implemented in terms of msplit
-- The following implementations are directly from the LogicT paper
ifte :: Member NdetEff r => Eff r a -> (a -> Eff r b) -> Eff r b -> Eff r b
ifte t th el = msplit t >>= check
  where check Nothing          = el
        check (Just (sg1,sg2)) = th sg1 `mplus` (sg2 >>= th)

once :: Member NdetEff r => Eff r a -> Eff r a
once m = msplit m >>= check
  where check Nothing        = mzero
        check (Just (sg1,_)) = return sg1

-- | called reflect in the LogicT paper
unmsplit :: Member NdetEff r => Maybe (a, Eff r a) -> Eff r a
unmsplit Nothing      = mzero
unmsplit (Just (a,m)) = return a `mplus` m
