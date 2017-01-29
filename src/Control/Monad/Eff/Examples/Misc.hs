{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Eff.Examples.Misc where

import Control.Monad.Eff
import Control.Monad.Eff.Reader
import Control.Monad.Eff.Writer
import Control.Monad.Eff.State
import Control.Monad.Eff.StateRW
import Control.Monad.Eff.NdetEff
import Control.Monad.Eff.Lift
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Trace

import Control.Monad
import Data.Maybe

---------------------------------------------------------------------------------
-- Reader Example
---------------------------------------------------------------------------------

type Bindings = [(String, Int)];

-- Returns True if the "count" variable contains correct bindings size.
isCountCorrect :: Bindings -> Bool
isCountCorrect bindings = run $ runReader bindings calcIsCountCorrect

-- The Reader monad, which implements this complicated check.
calcIsCountCorrect :: Member (Reader Bindings) r => Eff r Bool
calcIsCountCorrect = do
  count <- reader (lookupVar "count")
  (bindings :: Bindings) <- ask
  return (count == length bindings)

-- The selector function to  use with 'asks'.
-- Returns value of the variable with specified name.
lookupVar :: String -> Bindings -> Int
lookupVar name bindings = fromJust (lookup name bindings)

sampleBindings = [("count",3), ("1",1), ("b",2)]

exampleReader0 = do
  putStr $ "Count is correct for bindings " ++ show sampleBindings ++ ": "
  print (isCountCorrect sampleBindings)


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
  putStrLn $ "Modified 's' length: " ++ show modifiedLen
  putStrLn $ "Original 's' length: " ++ show len

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
addN n = foldl (>=>) return (replicate n addGet) 0

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
-- State Example
---------------------------------------------------------------------------------

ts1 :: Member (State Int) r => Eff r Int
ts1 = do
  put (10 :: Int)
  get

ts2 :: Member (State Int) r => Eff r Int
ts2 = do
  put (10::Int)
  x <- get
  put (20::Int)
  y <- get
  return (x+y)

exampleState1 :: (Int, Int)
exampleState1 = run $ runState (0::Int) ts1

exampleState1' :: (Int, Int)
exampleState1' = run $ runState' (0::Int) ts1

exampleState2 :: (Int, Int)
exampleState2 = run $ runState (0::Int) ts2

exampleState2' :: (Int, Int)
exampleState2' = run $ runState' (0::Int) ts2

---------------------------------------------------------------------------------
-- StateRW Example
---------------------------------------------------------------------------------

ts11 :: (Member (Reader Int) r, Member (Writer Int) r) => Eff r Int
ts11 = do
  tell (10 ::Int)
  x <- ask
  return (x::Int)

exampleStateRW1 = ((10,10) ==) $ run (runStateRW (0::Int) ts11)


ts21 :: (Member (Reader Int) r, Member (Writer Int) r) => Eff r Int
ts21 = do
  tell (10::Int)
  x <- ask
  tell (20::Int)
  y <- ask
  return (x+y)

exampleStateRW2= ((30,20) ==) $ run (runStateRW (0::Int) ts21)


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
           (const mzero)
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

---------------------------------------------------------------------------------
-- Lift Example
---------------------------------------------------------------------------------

tl1 :: (Member (Reader Int) r, MemberU2 Lift (Lift IO) r) => Eff r ()
tl1 = do
  (x::Int) <- ask
  lift (print x)

exampleLift1 :: IO ()
exampleLift1 = runLift . runReader (5::Int) $ tl1

mapMdebug' :: (Show a, MemberU2 Lift (Lift IO) r) => (a -> Eff r b) -> [a] -> Eff r [b]
mapMdebug' f [] = return []
mapMdebug' f (h:t) = do
  lift $ print h
  h' <- f h
  t' <- mapMdebug' f t
  return (h':t')

exampleLiftMapMdebug :: IO [Int]
exampleLiftMapMdebug = runLift . runReader (10::Int) $ mapMdebug' f [1..5]
  where f x = ask `add` return x

add :: Monad m => m Int -> m Int -> m Int
add = liftM2 (+)

---------------------------------------------------------------------------------
-- Exception Example
---------------------------------------------------------------------------------

et1 :: Eff r Int
et1 = return 1 `add` return 2

et1r = 3 == run et1

et2 :: Member (Exception Int) r => Eff r Int
et2 = return 1 `add` throwException (2::Int)
-- The following won't type: unhandled exception!
-- ex2rw = run et2
{-
    No instance for (Member (Exception Int) Void)
      arising from a use of `et2'
-}

-- The inferred type shows that ex21 is now pure
et21 :: Eff r (Either Int Int)
et21 = runException et2

exampleException21 = Left 2 == run et21

-- The example from the paper
newtype TooBig = TooBig Int deriving (Eq, Show)
-- The type is inferred
ex2 :: Member (Exception TooBig) r => Eff r Int -> Eff r Int
ex2 m = do
  v <- m
  if v > 5 then throwException (TooBig v)
     else return v

-- specialization to tell the type of the exception
runErrBig :: Eff (Exception TooBig ': r) a -> Eff r (Either TooBig a)
runErrBig = runException

ex2r = runReader (5::Int) (runErrBig (ex2 ask))

exampleException22 = Right 5 == run ex2r

exampleException221 = (Left (TooBig 7) ==) $
         run $ runReader (7::Int) (runErrBig (ex2 ask))

-- Different order of handlers (layers)
exampleException222 = (Left (TooBig 7) ==) $
         run $ runErrBig (runReader (7::Int) (ex2 ask))

exwr :: (Member (Writer String) r, Member (Exception Int) r) => Eff r Double
exwr = do
  tell "begin"
  (r::Double) <- throwException (10::Int)
  tell "end"
  return r

exwr1 :: Eff r (Either Int (Double, String))
exwr1 = runException (runWriter exwr)

exwr2 :: Eff r (Either Int Double, String)
exwr2 = runWriter . runException $ exwr

{-
exwrw1 :: Eff (Exception Int ': r) (Double, String)
exwrw1 = runWriter exwr

exwrw11 = catchException exwrw1 (\(e::Int) -> return (2.718, ""))

exwrw2 :: (Member (Writer String) r, Member (Exception Int) r) => Eff r Double
exwrw2 = catchException exwr (\(e::Int) -> return 3.14)
-}

exampleExceptionExwr1 = run exwr1
exampleExceptionExwr2 = run exwr2

---------------------------------------------------------------------------------
-- Exception Example
---------------------------------------------------------------------------------
mapMdebug :: (Show a, Member Trace r) => (a -> Eff r b) -> [a] -> Eff r [b]
mapMdebug f [] = return []
mapMdebug f (h:t) = do
  trace $ "mapMdebug: " ++ show h
  h' <- f h
  t' <- mapMdebug f t
  return (h':t')

testMapMdebugIO :: MemberU2 Lift (Lift IO) r => Eff r [Int]
testMapMdebugIO = runTrace $ runReader (10::Int) (mapMdebug f [1..5])
  where f x = ask `add` return x

testMapMdebugPure :: Eff r ([Int], [String])
testMapMdebugPure  = runTracePure $ runReader (10::Int) (mapMdebug f [1..5])
  where f x = ask `add` return x

exampleTraceIO = runLift testMapMdebugIO

exampleTracePure = run testMapMdebugPure

