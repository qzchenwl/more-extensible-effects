{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

-- Only for MemberU below, when emulating Monad Transformers
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}

-- Open unions (type-indexed co-products) for extensible effects
-- All operations are constant-time, and there is no Typeable constraint

-- This implementation relies on _closed_ type families added
-- to GHC 7.8
-- It has NO overlapping instances and NO Typeable
-- However, we sort of emulate typeable

-- Our list r of open union components is a small Universe.
-- Therefore, we can use

-- Our list r of open union components is a small Universe.
-- Therefore, we can use the Typeable-like evidence in that
-- universe. We hence can define
--
-- data Union r v where
--  Union :: t v -> TRep t r -> Union r v -- t is existential
-- where
-- data TRep t r where
--  T0 :: TRep t (t ': r)
--  TS :: TRep t r -> TRep (any ': r)
-- Then Member is a type class that produces TRep
-- Taken literally that approach doesn't seem much better than
-- OpenUinion41.hs. However, we can cheat and use the index of the
-- type t in the list r as the TRep. (We will need UnsafeCoerce then).

-- The interface is the same as of other OpenUnion*.hs
module Data.OpenUnion (
  Union, Member(..), MemberU2, decomp, weaken
) where

import Unsafe.Coerce(unsafeCoerce)
import GHC.TypeLits

-- The data constructors of Union are not exported

-- Strong Sum (Existential with the evidence) is an open union
-- t is can be a GADT and hence not necessarily a Functor.
-- Int is the index of t in the list r; that is, the index of t in the
-- universe r
data Union (r :: [ * -> * ]) v where
  Union :: !Int -> t v -> Union r v

{-# INLINE prj' #-}
{-# INLINE inj' #-}
inj' :: Int -> t v -> Union r v
inj' = Union

prj' :: Int -> Union r v -> Maybe (t v)
prj' n (Union n' x) | n == n'   = Just (unsafeCoerce x)
                    | otherwise = Nothing

data P (n::Nat) = P

class (KnownNat (FindElem t r)) => Member (t :: * -> *) r where
  inj :: t v -> Union r v
  prj :: Union r v -> Maybe (t v)

instance (KnownNat (FindElem t r)) => Member t r where
  inj = inj' (fromInteger $ natVal (P::P (FindElem t r)))
  prj = prj' (fromInteger $ natVal (P::P (FindElem t r)))


{-# INLINE decomp #-}
decomp :: Union (t ': r) v -> Either (Union r v) (t v)
decomp (Union 0 v) = Right $ unsafeCoerce v
decomp (Union n v) = Left  $ Union (n-1) v

weaken :: Union r w -> Union (any ': r) w
weaken (Union n v) = Union (n+1) v

-- Find an index of an element in a `list'
-- The element must exist
-- This closed type family disambiguates otherwise overlapping
-- instances
type family FindElem (t :: * -> *) r :: Nat where
  FindElem t (t ': r)  = 0
  FindElem t (any ': r)  = 1 + FindElem t r


type family EQU (a :: k) (b :: k) :: Bool where
  EQU a a = True
  EQU a b = False

-- This class is used for emulating monad transformers
class Member t r => MemberU2 (tag :: k -> * -> *) (t :: * -> *) r | tag r -> t

instance (MemberU' (EQU t1 t2) tag t1 (t2 ': r)) => MemberU2 tag t1 (t2 ': r)

class Member t r => MemberU' (f::Bool) (tag :: k -> * -> *) (t :: * -> *) r | tag r -> t

instance MemberU' True tag (tag e) (tag e ': r)

instance (Member t (t' ': r), MemberU2 tag t r) => MemberU' False tag t (t' ': r)
