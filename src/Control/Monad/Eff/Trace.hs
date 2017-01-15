{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Control.Monad.Eff.Trace (
  Trace,
  trace,
  runTracePure,
  runTrace
) where

import Control.Monad.Eff
import Control.Monad.Eff.Lift

data Trace v where
  Trace :: String -> Trace ()

trace :: Member Trace r => String -> Eff r ()
trace = send . Trace

runTracePure :: Eff (Trace ': r) a -> Eff r (a, [String])
runTracePure = handleRelay (\x -> return (x, [])) (\(Trace s) k -> k () >>= \(a, ss) -> return (a, s:ss))

runTrace :: MemberU2 Lift (Lift IO) r => Eff (Trace ': r) a -> Eff r a
runTrace = handleRelay return (\(Trace s) k -> lift (putStrLn s) >>= k)
