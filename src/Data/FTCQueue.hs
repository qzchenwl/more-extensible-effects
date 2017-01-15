{-# LANGUAGE GADTs #-}

-- Fast type-aligned queue optimized to effectful functions
-- (a -> m b)
-- (monad continuations have this type).
-- Constant-time append and snoc and
-- average constant-time left-edge deconstruction

module Data.FTCQueue (
  FTCQueue,
  tsingleton,
  (|>), -- snoc
  (><), -- append
  ViewL(..),
  tviewl
) where

-- Non-empty tree. Deconstruction operations make it more and more
-- left-leaning

data FTCQueue m a b where
  Leaf :: (a -> m b) -> FTCQueue m a b
  Node :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b


-- Exported operations

-- There is no tempty: use (tsingleton return), which works just the same.
-- The names are chosen for compatibility with FastTCQueue

{-# INLINE tsingleton #-}
tsingleton :: (a -> m b) -> FTCQueue m a b
tsingleton r = Leaf r

-- snoc: clearly constant-time
{-# INLINE (|>) #-}
(|>) :: FTCQueue m a x -> (x -> m b) -> FTCQueue m a b
t |> r = Node t (Leaf r)

-- append: clearly constant-time
{-# INLINE (><) #-}
(><) :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b
t1 >< t2 = Node t1 t2


-- Left-edge deconstruction
data ViewL m a b where
  TOne  :: (a -> m b) -> ViewL m a b
  (:|)  :: (a -> m x) -> (FTCQueue m x b) -> ViewL m a b

{-# INLINABLE tviewl #-}
tviewl :: FTCQueue m a b -> ViewL m a b
tviewl (Leaf r) = TOne r
tviewl (Node t1 t2) = go t1 t2
  where
    go :: FTCQueue m a x -> FTCQueue m x b -> ViewL m a b
    go (Leaf r) tr = r :| tr
    go (Node tl1 tl2) tr = go tl1 (Node tl2 tr)

