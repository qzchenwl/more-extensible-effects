{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Eff.Examples where

import Control.Monad.Eff
import Control.Monad.Eff.Reader
import Control.Monad.Eff.Writer
import Control.Monad.Eff.NdetEff

import Control.Monad
import Data.Maybe

---------------------------------------------------------------------------------
-- Reader Example
---------------------------------------------------------------------------------

type Bindings = [(String, Int)];

-- Returns True if the "count" variable contains correct bindings size.
isCountCorrect :: Bindings -> Bool
isCountCorrect bindings = run $ runReader bindings calc_isCountCorrect

-- The Reader monad, which implements this complicated check.
calc_isCountCorrect :: Member (Reader Bindings) r => Eff r Bool
calc_isCountCorrect = do
  count <- reader (lookupVar "count")
  (bindings :: Bindings) <- ask
  return (count == (length bindings))

-- The selector function to  use with 'asks'.
-- Returns value of the variable with specified name.
lookupVar :: String -> Bindings -> Int
lookupVar name bindings = fromJust (lookup name bindings)

sampleBindings = [("count",3), ("1",1), ("b",2)]

exampleReader0 = do
  putStr $ "Count is correct for bindings " ++ (show sampleBindings) ++ ": ";
  putStrLn $ show (isCountCorrect sampleBindings);



calculateContentLen :: Member (Reader String) r => Eff r Int
calculateContentLen = do
  (content :: String) <- ask
  return (length content)

-- Calls calculateContentLen after adding a prefix to the Reader content.
calculateModifiedContentLen :: Member (Reader String) r => Eff r Int
calculateModifiedContentLen = local ("Prefix " ++) calculateContentLen

exampleReader1 = do
  let s = "12345"
  let modifiedLen = run . runReader s $ calculateModifiedContentLen
  let len = run . runReader s $ calculateContentLen
  putStrLn $ "Modified 's' length: " ++ (show modifiedLen)
  putStrLn $ "Original 's' length: " ++ (show len)

---------------------------------------------------------------------------------
-- Writer Example
---------------------------------------------------------------------------------

simpleWriter :: Member (Writer String) r => Eff r Int
simpleWriter = do
  tell "i = 1\n"
  let i = 1
  tell "j = 2\n"
  let j = 2
  tell $ "return i + j = " ++ show (i+j)
  return (i+j)

exampleWriter0 = do
  let (a::Int, logs::String) = run . runWriter $ simpleWriter
  print a
  putStrLn logs

addGet :: Member (Reader Int) r => Int -> Eff r Int
addGet x = do
  i <- ask
  return (i+x)

addN :: Member (Reader Int) r => Int -> Eff r Int
addN n = foldl (>>>) return (replicate n addGet) 0

f >>> k = \x -> f x >>= k

rdwr :: (Member (Reader Int) r, Member (Writer String) r)
  => Eff r Int
rdwr = do
  tell "begin\n"
  r <- addN 10
  tell "end\n"
  return r

exampleWriter1 = do
  let (result::Int, logs::String) = run . runReader (3::Int) . runWriter $ rdwr
  print result
  putStrLn logs
  let (result'::Int, logs'::String) = run . runWriter . runReader (3::Int) $ rdwr
  print result'
  putStrLn logs'

---------------------------------------------------------------------------------
-- NdetEff Example
---------------------------------------------------------------------------------
testCA :: MonadPlus m => m Int
testCA = do
  i <- msum . fmap return $ [1..10]
  guard (i `mod` 2 == 0)
  return i

exampleNdetEffChoiceA :: [Int]
exampleNdetEffChoiceA = run . makeChoiceA $ testCA

-- | primes (very inefficiently -- but a good example of ifte)
testIfte :: Member NdetEff r => Eff r Int
testIfte = do
  n <- gen
  ifte (do { d <- gen; guard $ d < n && n `mod` d == 0 })
           (\_ -> mzero)
           (return n)
  where gen = msum . fmap return $ [2..30]

exampleNdetEffIfte :: [Int]
exampleNdetEffIfte = run . makeChoiceA $ testIfte

tsplit :: (Member (Writer String) r, Member NdetEff r) => Eff r Int
tsplit =
  (tell "begin" >> return 1) `mplus`
  (tell "end"   >> return 2)

exampleNdetEffTsplit10, exampleNdetEffTsplit11 :: ([Int],String)
exampleNdetEffTsplit10 = run $ runWriter $ makeChoiceA tsplit
exampleNdetEffTsplit11 = run $ runWriter $ makeChoiceA (msplit tsplit >>= unmsplit)

exampleNdetEffTsplit20, exampleNdetEffTsplit21 :: [(Int,String)]
exampleNdetEffTsplit20 = run $ makeChoiceA $ runWriter tsplit
exampleNdetEffTsplit21 = run $ makeChoiceA $ runWriter (msplit tsplit >>= unmsplit)

