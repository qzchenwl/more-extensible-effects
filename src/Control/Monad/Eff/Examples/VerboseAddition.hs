{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
runLogger = handleRelay ret handle
  where
    ret :: a -> Eff r (a, [String])
    ret x = return (x, [])
    handle :: Handler Log r (a, [String])
    handle (Log s) k = do
      (x, ss) <- k ()
      return (x, s:ss)

runIOLogger :: forall r a. MemberU2 Lift (Lift IO) r => Eff (Log ': r) a -> Eff r a
runIOLogger = handleRelay ret handle
  where
    ret :: a -> Eff r a
    ret = return
    handle :: Handler Log r a
    handle (Log s) k = lift (putStrLn s) >>= k

example :: Member Log r => Eff r Int
example = do
  log "I'm starting with 1..."
  let x = 1

  log "and I'm adding 2..."
  let y = 2

  let r = x + y
  log $ "Looks like the result is " ++ show r
  return r

