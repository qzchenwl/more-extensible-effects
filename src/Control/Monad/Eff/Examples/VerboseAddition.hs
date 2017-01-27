{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Eff.Examples.VerboseAddition where

import Control.Monad.Eff
import Control.Monad.Eff.Lift
import Prelude hiding (log)

data Log v where
  Log :: String -> Log ()

log :: Member Log r => String -> Eff r ()
log = send . Log

runLogger :: Eff (Log ': r) a -> Eff r (a, [String])
runLogger = handleRelay (\x -> return (x, []))
                        (\(Log s) k -> k () >>= \(x, ss) -> return (x, s:ss))

runIOLogger :: MemberU2 Lift (Lift IO) r => Eff (Log ': r) a -> Eff r a
runIOLogger = handleRelay return
                          (\(Log s) k -> lift (putStrLn s) >>= k)

verboseAddition :: Member Log r => Eff r Int
verboseAddition = do
  log "I'm starting with 1..."
  x <- return 1

  log "and I'm adding 2..."
  y <- return 2

  let r = x + y

  log $ "Looks like the result is " ++ show r
  return r
